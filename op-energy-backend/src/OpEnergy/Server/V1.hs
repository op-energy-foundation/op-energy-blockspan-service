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
module OpEnergy.Server.V1
  ( server
  , schedulerIteration
  )where

import           Servant
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad(forM)
import           Control.Monad.Reader(ask)
import           Control.Monad.Logger(logError)
import qualified Data.Text as T
import qualified Control.Concurrent.STM.TVar as TVar

import           Data.OpEnergy.API.V1
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Account
import           Data.OpEnergy.API.V1.Positive
import qualified OpEnergy.Server.GitCommitHash as Server
import qualified OpEnergy.Server.V1.Metrics as Metrics( MetricsState(..))
import           OpEnergy.Server.V1.Class (AppT, runLogging, State(..))
import           OpEnergy.Server.V1.BlockHeadersService(syncBlockHeaders, getBlockHeaderByHash, getBlockHeaderByHeight, mgetBlockHeaderByHeight)
import           OpEnergy.Server.V1.WebSocketService(webSocketConnection)
import           OpEnergy.Server.V1.BlockSpanService(getBlockSpanListByRange, getBlockSpanList)
import           OpEnergy.Server.V1.StatisticsService(calculateStatistics, getTheoreticalActualMTPPercents)
import           Data.Text.Show(tshow)

import           Prometheus(MonadMonitor)
import qualified Prometheus as P


-- | here goes implementation of OpEnergy API, which should match Data.OpEnergy.API.V1.V1API
server:: ServerT V1API (AppT Handler)
server = OpEnergy.Server.V1.WebSocketService.webSocketConnection
    :<|> registerPost
    :<|> loginPost
    :<|> strikeMediantimeGet
    :<|> strikeBlockMediantimeGet
    :<|> strikeMediantimePost
    :<|> slowFastGuessMediantimeGet
    :<|> slowFastGuessMediantimePost
    :<|> strikeHistoryMediantimeGet
    :<|> slowFastResultsMediantimeGet
    :<|> userDisplayNamePost
    :<|> OpEnergy.Server.V1.StatisticsService.calculateStatistics
    :<|> OpEnergy.Server.V1.BlockHeadersService.getBlockHeaderByHash
    :<|> OpEnergy.Server.V1.BlockHeadersService.getBlockHeaderByHeight
    :<|> getBlocksByBlockSpan
    :<|> getBlocksWithNbdrByBlockSpan
    :<|> getBlocksWithHashrateByBlockSpan
    :<|> OpEnergy.Server.V1.BlockSpanService.getBlockSpanList
    :<|> oeGitHashGet

-- | one iteration that called from scheduler thread
schedulerIteration :: (MonadIO m, MonadMonitor m) => AppT m ()
schedulerIteration = OpEnergy.Server.V1.BlockHeadersService.syncBlockHeaders

getBlocksByBlockSpan
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockHeight
  -> Positive Int
  -> Maybe (Positive Int)
  -> AppT m [[BlockHeader]]
getBlocksByBlockSpan startHeight span mNumberOfSpan = do
  State{ metrics = Metrics.MetricsState{ getBlocksByBlockSpan = getBlocksByBlockSpan}
       , currentTip = currentTipV
       } <- ask
  P.observeDuration getBlocksByBlockSpan $ do
    mCurrentTip <- liftIO $! TVar.readTVarIO currentTipV
    spans <- case mNumberOfSpan of
      Just numberOfSpan -> OpEnergy.Server.V1.BlockSpanService.getBlockSpanList startHeight span numberOfSpan
      Nothing-> case mCurrentTip of
        Just currentTip -> OpEnergy.Server.V1.BlockSpanService.getBlockSpanListByRange startHeight (blockHeaderHeight currentTip) span
        Nothing -> do
          let err = "ERROR: getBlocksByBlockSpan: no current tip had been discovered yet"
          runLogging $ $(logError) err
          error $ T.unpack err
    forM spans $ \(BlockSpan startHeight endHeight)-> do
      mstart <- OpEnergy.Server.V1.BlockHeadersService.mgetBlockHeaderByHeight startHeight
      mend <- OpEnergy.Server.V1.BlockHeadersService.mgetBlockHeaderByHeight endHeight
      case (mstart, mend) of
        (Just start, Just end ) -> return [ start, end]
        _ -> do
          let err = "failed to get block headers for block span {" <> tshow startHeight <> ", " <> tshow endHeight <> "}"
          runLogging $ $(logError) err
          error $ T.unpack err

getBlocksWithNbdrByBlockSpan
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockHeight
  -> Positive Int
  -> Maybe (Positive Int)
  -> AppT m [BlockSpanHeadersNbdr]
getBlocksWithNbdrByBlockSpan startHeight span mNumberOfSpans = do
  State{ metrics = Metrics.MetricsState{ getBlocksWithNbdrByBlockSpan = getBlocksWithNbdrByBlockSpan} } <- ask
  P.observeDuration getBlocksWithNbdrByBlockSpan $ do
    blockSpansBlocks <- getBlocksByBlockSpan startHeight span mNumberOfSpans
    return $! map toBlockSpanHeadersNbdr blockSpansBlocks
  where
    toBlockSpanHeadersNbdr (startBlock:endBlock:_) = BlockSpanHeadersNbdr
      { startBlock = startBlock
      , endBlock = endBlock
      , Data.OpEnergy.API.V1.nbdr = getTheoreticalActualMTPPercents startBlock endBlock
      }
    toBlockSpanHeadersNbdr _ = error "getBlocksWithNbdrByBlockSpan: unexpected arguments"

getBlocksWithHashrateByBlockSpan
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockHeight
  -> Positive Int
  -> Maybe (Positive Int)
  -> AppT m [BlockSpanHeadersHashrate]
getBlocksWithHashrateByBlockSpan startHeight span mNumberOfSpans = do
  State{ metrics = Metrics.MetricsState{ getBlocksWithHashrateByBlockSpan = getBlocksWithHashrateByBlockSpan} } <- ask
  P.observeDuration getBlocksWithHashrateByBlockSpan $ do
    blockSpansBlocks <- getBlocksByBlockSpan startHeight span mNumberOfSpans
    return $! map toBlockSpanHeadersHashrate blockSpansBlocks
  where
    toBlockSpanHeadersHashrate (startBlock:endBlock:_) = BlockSpanHeadersHashrate
      { startBlock = startBlock
      , endBlock = endBlock
      , Data.OpEnergy.API.V1.hashrate = toHashrate startBlock endBlock
      }
    toBlockSpanHeadersHashrate _ = error "getBlocksWithHashrateByBlockSpan: unexpected arguments"
    toHashrate startBlock endBlock = (blockHeaderChainwork endBlock - blockHeaderChainwork startBlock) `div` (fromIntegral (blockHeaderMediantime endBlock - blockHeaderMediantime startBlock))

-- returns just commit hash, provided by build system
oeGitHashGet :: AppT Handler GitHashResponse
oeGitHashGet = return $ GitHashResponse
  { gitCommitHash = Server.gitCommitHash
  }

{- here goes a set of unimplemented yet handlers --> -}
registerPost :: AppT Handler RegisterResult
registerPost = undefined

loginPost :: [AccountSecret]-> AppT Handler [AccountToken]
loginPost = undefined

strikeMediantimeGet :: AppT Handler [TimeStrike]
strikeMediantimeGet = undefined

strikeBlockMediantimeGet :: BlockHeight-> AppT Handler [TimeStrike]
strikeBlockMediantimeGet = undefined

strikeMediantimePost :: CreateTimeStrikeRequest -> AppT Handler TimeStrike
strikeMediantimePost = undefined

slowFastGuessMediantimeGet :: BlockHeight-> NLockTime-> AppT Handler [SlowFastGuess]
slowFastGuessMediantimeGet = undefined

slowFastGuessMediantimePost :: CreateSlowFastGuessRequest -> AppT Handler SlowFastGuess
slowFastGuessMediantimePost = undefined

strikeHistoryMediantimeGet :: AppT Handler [TimeStrikeHistory]
strikeHistoryMediantimeGet = undefined

slowFastResultsMediantimeGet :: [AccountToken]-> NLockTime-> BlockHeight-> AppT Handler [SlowFastResult]
slowFastResultsMediantimeGet = undefined

userDisplayNamePost :: PostUserDisplayNameRequest -> AppT Handler ()
userDisplayNamePost = undefined
