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
import           Data.Maybe(fromJust)

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
    :<|> getBlocksByBlockSpan
    :<|> getBlocksWithNbdrByBlockSpan
    :<|> getBlocksWithHashrateByBlockSpan
    :<|> OpEnergy.Server.V1.BlockSpanService.getBlockSpanList
    :<|> getSingleBlockspan
    :<|> oeGitHashGet

-- | one iteration that called from scheduler thread
schedulerIteration :: (MonadIO m, MonadMonitor m) => AppT m ()
schedulerIteration = OpEnergy.Server.V1.BlockHeadersService.syncBlockHeaders

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

getBlocksWithHashrateByBlockSpan
  :: BlockHeight
  -> Positive Int
  -> Maybe (Positive Int)
  -> AppM [BlockSpanHeadersHashrate]
getBlocksWithHashrateByBlockSpan startHeight span mNumberOfSpans = do
  State{ metrics = Metrics.MetricsState{ getBlocksWithHashrateByBlockSpan = getBlocksWithHashrateByBlockSpan} } <- ask
  P.observeDuration getBlocksWithHashrateByBlockSpan $ do
    blockSpansBlocks <- getBlocksByBlockSpan startHeight span mNumberOfSpans
    return $! map toBlockSpanHeadersHashrate blockSpansBlocks
  where
    toBlockSpanHeadersHashrate (BlockSpanHeadersNbdrHashrate {..}) = BlockSpanHeadersHashrate
      { startBlock = startBlock
      , endBlock = endBlock
      , Data.OpEnergy.API.V1.hashrate = fromJust hashrate
      }

-- | Returns a single blockspan ending at the specified block height
getSingleBlockspan
  :: BlockHeight
  -> Maybe (Positive Int)
  -> AppM BlockSpanHeadersNbdrHashrate
getSingleBlockspan blockHeight mSpanSize = do
  State{ metrics = Metrics.MetricsState{ getSingleBlockspan = getSingleBlockspanH} } <- ask
  P.observeDuration getSingleBlockspanH $ do
    let spanSize = case mSpanSize of
          Just size -> size
          Nothing -> defaultSpanSize
    
    -- Validate that blockHeight is sufficient for the requested span
    let spanSizeNat = naturalFromPositive spanSize
    if blockHeight < spanSizeNat
      then do
        let err = "ERROR: getSingleBlockspan: block height " <> tshow blockHeight <> " is too low for span size " <> tshow (fromPositive spanSize) <> " (minimum required: " <> tshow spanSizeNat <> ")"
        runLogging $ $(logError) err
        throwJSON err400 err
      else do
        -- Calculate start height: blockHeight - spanSize + 1
        let startHeight = blockHeight - spanSizeNat + 1
        -- Use existing logic but request only 1 span
        blockSpansBlocks <- getBlocksByBlockSpan startHeight spanSize (Just $ verifyPositive 1)
        case blockSpansBlocks of
          [singleBlockspan] -> return singleBlockspan
          [] -> do
            let err = "ERROR: getSingleBlockspan: no blockspan found for block height " <> tshow blockHeight
            runLogging $ $(logError) err
            throwJSON err400 err
          _ -> do
            let err = "ERROR: getSingleBlockspan: unexpected multiple blockspans returned for single blockspan request"
            runLogging $ $(logError) err
            throwJSON err500 err

-- returns just commit hash, provided by build system
oeGitHashGet :: AppT Handler GitHashResponse
oeGitHashGet = return $ GitHashResponse
  { gitCommitHash = Server.gitCommitHash
  }

