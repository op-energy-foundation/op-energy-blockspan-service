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

import           Data.Text(Text)

import           Servant
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad(forM)
import           Control.Monad.Reader( asks)
import           Control.Monad.Trans.Except (ExceptT(..), throwE)
import           Control.Monad.Trans.Class(lift)
import qualified Control.Concurrent.STM.TVar as TVar
import           Data.Maybe(fromJust, fromMaybe)

import           Data.OpEnergy.API
import           Data.OpEnergy.API.V1
import qualified Data.OpEnergy.API.V2.BlockSpanSummary as V2
import           Data.OpEnergy.API.V2 (V2API)
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Positive
import qualified OpEnergy.Server.GitCommitHash as Server
import           OpEnergy.Server.V1.Class
                 as State(AppT, AppM, State(..), profile)
import qualified OpEnergy.Server.V1.BlockHeadersService
                 as V1 ( syncBlockHeaders
                       , getBlockHeaderByHash
                       , getBlockHeaderByHeight
                       , mgetBlockHeaderByHeight
                       )
import           OpEnergy.Server.V1.WebSocketService(webSocketConnection)
import           OpEnergy.Server.V1.BlockSpanService(getBlockSpanListByRange, getBlockSpanList)
import           OpEnergy.Server.V1.StatisticsService(calculateStatistics, getTheoreticalActualMTPPercents)
import           OpEnergy.Server.Common
                 ( eitherLogThrowOrReturn
                 , runExceptPrefixT
                 , exceptTMaybeT
                 , eitherException
                 )
import           Data.Text.Show(tshow)

import           Prometheus(MonadMonitor)

-- | Default span size for single blockspan queries
defaultSpanSize :: Positive Int
defaultSpanSize = verifyPositive 24

websocketHandler :: ServerT WebSocketAPI (AppT Handler)
websocketHandler = OpEnergy.Server.V1.WebSocketService.webSocketConnection
  :<|> OpEnergy.Server.V1.WebSocketService.webSocketConnection

-- | here goes implementation of OpEnergy API, which should match Data.OpEnergy.API.V2.V2API
server:: ServerT V2API (AppT Handler)
server = OpEnergy.Server.V1.StatisticsService.calculateStatistics
    :<|> V1.getBlockHeaderByHash
    :<|> V1.getBlockHeaderByHeight
    :<|> getBlocksWithNbdrByBlockSpan
    :<|> getMultipleBlockspans
    :<|> getSingleBlockspan
    :<|> oeGitHashGet

-- | one iteration that called from scheduler thread
schedulerIteration :: (MonadIO m, MonadMonitor m) => AppT m ()
schedulerIteration = V1.syncBlockHeaders

-- Internal helper function (not exposed as API)
getBlocksByBlockSpan
  :: BlockHeight
  -> Positive Int
  -> Maybe (Positive Int)
  -> AppM (Either Text [BlockSpanHeadersNbdrHashrate])
getBlocksByBlockSpan startHeight span mNumberOfSpan =
    let name = "getBlocksByBlockSpan"
    in profile name $ runExceptPrefixT name $ do
  currentTipV <- lift $ asks State.currentTip
  currentTip <- exceptTMaybeT "no current tip had been discovered yet"
    $ liftIO $! TVar.readTVarIO currentTipV
  spans <- case mNumberOfSpan of
    Just numberOfSpan -> ExceptT $ eitherException
      $ OpEnergy.Server.V1.BlockSpanService.getBlockSpanList startHeight span
        numberOfSpan
    Nothing-> ExceptT $ eitherException
      $ OpEnergy.Server.V1.BlockSpanService.getBlockSpanListByRange startHeight
        (blockHeaderHeight currentTip) span
  forM spans $ \(BlockSpan startHeight endHeight)-> do
    mstart <- ExceptT $ eitherException $ V1.mgetBlockHeaderByHeight startHeight
    mend <- ExceptT $ eitherException $ V1.mgetBlockHeaderByHeight endHeight
    (start, end) <- exceptTMaybeT
      ( "failed to get block headers for block span {"
      <> tshow startHeight <> ", " <> tshow endHeight <> "}"
      )
      $ return $ do
        start <- mstart
        end <- mend
        return (start, end)
    let
        mNbdr = Just $! getTheoreticalActualMTPPercents start end
        mHashrate = Just $!
          (blockHeaderChainwork end - blockHeaderChainwork start)
          `div`
          (fromIntegral (blockHeaderMediantime end - blockHeaderMediantime start))
    return $! BlockSpanHeadersNbdrHashrate
      { startBlock = start
      , endBlock = end
      , nbdr = mNbdr
      , hashrate = mHashrate
      }

getBlocksWithNbdrByBlockSpan
  :: BlockHeight
  -> Positive Int
  -> Maybe (Positive Int)
  -> AppM [BlockSpanHeadersNbdr]
getBlocksWithNbdrByBlockSpan startHeight span mNumberOfSpans =
    let name = "V2.getBlocksWithNbdrByBlockSpan"
    in profile name $ eitherLogThrowOrReturn $ runExceptPrefixT name $ do
  blockSpansBlocks <- ExceptT $ getBlocksByBlockSpan startHeight span
    mNumberOfSpans
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
    let name = "V2.getSingleBlockspan"
    in profile name $ eitherLogThrowOrReturn $ runExceptPrefixT name $ do
  let
      spanSize = fromMaybe defaultSpanSize mSpanSize
      -- Validate that blockHeight is sufficient for the requested span
      spanSizeNat = naturalFromPositive spanSize
  startHeight <- if blockHeight < spanSizeNat
    then throwE $ "block height " <> tshow blockHeight
             <> " is too low for span size " <> tshow (fromPositive spanSize)
             <> " (minimum required: " <> tshow spanSizeNat <> ")"
      -- Calculate start height: blockHeight - spanSize
    else return $! blockHeight - spanSizeNat
  -- Use existing logic but request only 1 span
  positive1 <- ExceptT $ return $ verifyPositiveEither 1
  blockSpansBlocks <- ExceptT $ getBlocksByBlockSpan
                        startHeight
                        spanSize
                        (Just positive1)
  case blockSpansBlocks of
    [singleBlockspan] -> return singleBlockspan
    [] -> throwE $! "no blockspan found for block height "
                 <> tshow blockHeight
    _ -> throwE "unexpected multiple blockspans returned for single blockspan \
                \request"

getMultipleBlockspans
  :: BlockHeight
  -> Positive Int
  -> Maybe (Positive Int)
  -> Maybe Bool
  -> AppM (Either [V2.BlockSpanSummary] [BlockSpanHeadersNbdrHashrate])
getMultipleBlockspans startHeight spanSize mNumberOfSpans mWithHeaders =
    let name = "getMultipleBlockspans"
    in profile name $ eitherLogThrowOrReturn $ runExceptPrefixT name $ do
  fullBlockspans <- ExceptT $ getBlocksByBlockSpan startHeight spanSize mNumberOfSpans
  let withHeaders = case mWithHeaders of
        Just False -> False
        _ -> True
  return $! if withHeaders
    then Right fullBlockspans
    else Left $! map toSummary fullBlockspans
  where
    toSummary (BlockSpanHeadersNbdrHashrate {..}) = V2.BlockSpanSummary
      { startBlockHeight = blockHeaderHeight startBlock
      , endBlockHeight = blockHeaderHeight endBlock
      , V2.nbdr = fromJust nbdr
      , V2.hashrate = fromJust hashrate
      }

-- returns just commit hash, provided by build system
oeGitHashGet :: AppT Handler GitHashResponse
oeGitHashGet = return $ GitHashResponse
  { gitCommitHash = Server.gitCommitHash
  }

