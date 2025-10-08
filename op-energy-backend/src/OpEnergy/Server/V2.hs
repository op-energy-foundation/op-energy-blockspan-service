{-- |
 - This module is the top module of backend V1
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleContexts           #-}
module OpEnergy.Server.V2
  ( websocketHandler
  , schedulerIteration
  , server
  )where

import           Servant
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad(forM)
import           Control.Monad.Reader(ask)
import           Control.Monad.Logger(logError)
import qualified Control.Concurrent.STM.TVar as TVar
import           Data.Maybe(fromJust, fromMaybe)

import           Data.OpEnergy.API
import           Data.OpEnergy.API.V1
import           Data.OpEnergy.API.V2
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Positive
import qualified OpEnergy.Server.GitCommitHash as Server
import qualified OpEnergy.Server.V1.Metrics as Metrics( MetricsState(..))
import           OpEnergy.Server.V1.Class (AppT, AppM, runLogging, State(..))
import           OpEnergy.Server.V1.BlockHeadersService(syncBlockHeaders, getBlockHeaderByHash, getBlockHeaderByHeight, mgetBlockHeaderByHeight)
import           OpEnergy.Server.V1.WebSocketService(webSocketConnection)
import           OpEnergy.Server.V1.BlockSpanService(getBlockSpanListByRange, getBlockSpanList)
import           OpEnergy.Server.V1.StatisticsService(calculateStatistics, getTheoreticalActualMTPPercents)
import           Data.Text.Show(tshow)

import           Prometheus(MonadMonitor)
import qualified Prometheus as P
import           Data.OpEnergy.API.V1.Error(throwJSON)

-- | Default span size for single blockspan queries
defaultSpanSize :: Positive Int
defaultSpanSize = verifyPositive 24

websocketHandler :: ServerT WebSocketAPI (AppT Handler)
websocketHandler = OpEnergy.Server.V1.WebSocketService.webSocketConnection
  :<|> OpEnergy.Server.V1.WebSocketService.webSocketConnection

-- | here goes implementation of OpEnergy API, which should match Data.OpEnergy.API.V2.V2API
server:: ServerT V2API (AppT Handler)
server = OpEnergy.Server.V1.StatisticsService.calculateStatistics
    :<|> OpEnergy.Server.V1.BlockHeadersService.getBlockHeaderByHash
    :<|> OpEnergy.Server.V1.BlockHeadersService.getBlockHeaderByHeight
    :<|> getBlocksWithNbdrByBlockSpan
    :<|> OpEnergy.Server.V1.BlockSpanService.getBlockSpanList
    :<|> getSingleBlockspan
    :<|> oeGitHashGet

-- | one iteration that called from scheduler thread
schedulerIteration :: (MonadIO m, MonadMonitor m) => AppT m ()
schedulerIteration = OpEnergy.Server.V1.BlockHeadersService.syncBlockHeaders

-- Internal helper function (not exposed as API)
getBlocksByBlockSpan
  :: BlockHeight
  -> Positive Int
  -> Maybe (Positive Int)
  -> AppM [BlockSpanHeadersNbdrHashrate]
getBlocksByBlockSpan startHeight span mNumberOfSpan = do
  State{ metrics = Metrics.MetricsState{ getBlocksByBlockSpan = getBlocksByBlockSpan}
       , currentTip = currentTipV
       } <- ask
  P.observeDuration getBlocksByBlockSpan $ do
    mCurrentTip <- liftIO $! TVar.readTVarIO currentTipV
    let withNbdr = True
        withHashrate = True
    spans <- case mNumberOfSpan of
      Just numberOfSpan -> OpEnergy.Server.V1.BlockSpanService.getBlockSpanList startHeight span numberOfSpan
      Nothing-> case mCurrentTip of
        Just currentTip -> OpEnergy.Server.V1.BlockSpanService.getBlockSpanListByRange startHeight (blockHeaderHeight currentTip) span
        Nothing -> do
          let err = "ERROR: getBlocksByBlockSpan: no current tip had been discovered yet"
          runLogging $ $(logError) err
          throwJSON err400 err
    forM spans $ \(BlockSpan startHeight endHeight)-> do
      mstart <- OpEnergy.Server.V1.BlockHeadersService.mgetBlockHeaderByHeight startHeight
      mend <- OpEnergy.Server.V1.BlockHeadersService.mgetBlockHeaderByHeight endHeight
      case (mstart, mend) of
        (Just start, Just end ) -> do
          mNbdr <- if withNbdr
            then return $! Just $! getTheoreticalActualMTPPercents start end
            else return Nothing
          mHashrate <- if withHashrate
            then return $! Just $! (blockHeaderChainwork end - blockHeaderChainwork start) `div` (fromIntegral (blockHeaderMediantime end - blockHeaderMediantime start))
            else return Nothing
          return $! BlockSpanHeadersNbdrHashrate
            { startBlock = start
            , endBlock = end
            , nbdr = mNbdr
            , hashrate = mHashrate
            }
        _ -> do
          let err = "ERROR: getBlocksByBlockSpan: failed to get block headers for block span {" <> tshow startHeight <> ", " <> tshow endHeight <> "}"
          runLogging $ $(logError) err
          throwJSON err400 err

getBlocksWithNbdrByBlockSpan
  :: BlockHeight
  -> Positive Int
  -> Maybe (Positive Int)
  -> AppM [BlockSpanHeadersNbdr]
getBlocksWithNbdrByBlockSpan startHeight span mNumberOfSpans = do
  State{ metrics = Metrics.MetricsState{ getBlocksWithNbdrByBlockSpan = getBlocksWithNbdrByBlockSpan} } <- ask
  P.observeDuration getBlocksWithNbdrByBlockSpan $ do
    blockSpansBlocks <- getBlocksByBlockSpan startHeight span mNumberOfSpans
    return $! map toBlockSpanHeadersNbdr blockSpansBlocks
  where
    toBlockSpanHeadersNbdr (BlockSpanHeadersNbdrHashrate {..}) = BlockSpanHeadersNbdr
      { startBlock = startBlock
      , endBlock = endBlock
      , Data.OpEnergy.API.V1.nbdr = fromJust $! nbdr
      }

getSingleBlockspan
  :: BlockHeight
  -> Maybe (Positive Int)
  -> AppM BlockSpanHeadersNbdrHashrate
getSingleBlockspan blockHeight mSpanSize =
    let name = "getSingleBlockspan"
    in profile name $ eitherLogThrowOrReturn $ runExceptPrefixT name $ do
  let
      spanSize = fromMaybe defaultSpanSize mSpanSize
      -- Validate that blockHeight is sufficient for the requested span
      spanSizeNat = naturalFromPositive spanSize
  startHeight <- if blockHeight < spanSizeNat
    then throwE $ "ERROR: getSingleBlockspan: block height " <> tshow blockHeight
             <> " is too low for span size " <> tshow (fromPositive spanSize)
             <> " (minimum required: " <> tshow spanSizeNat <> ")"
      -- Calculate start height: blockHeight - spanSize + 1
    else return $! blockHeight - spanSizeNat + 1
  -- Use existing logic but request only 1 span
  positive1 <- ExceptT $ return $ verifyPositiveEither 1
  blockSpansBlocks <- lift $ getBlocksByBlockSpan
                        startHeight
                        spanSize
                        (Just positive1)
  case blockSpansBlocks of
    [singleBlockspan] -> return singleBlockspan
    [] -> throwE $! "ERROR: getSingleBlockspan: no blockspan found for block height "
                 <> tshow blockHeight
    _ -> throwE "ERROR: getSingleBlockspan: unexpected multiple blockspans \
                \returned for single blockspan request"
  where
  profile _reservedForFutureUse func = do
    State{ metrics = Metrics.MetricsState{ getSingleBlockspan = getSingleBlockspanH} } <- ask
    P.observeDuration getSingleBlockspanH func

-- returns just commit hash, provided by build system
oeGitHashGet :: AppT Handler GitHashResponse
oeGitHashGet = return $ GitHashResponse
  { gitCommitHash = Server.gitCommitHash
  }

