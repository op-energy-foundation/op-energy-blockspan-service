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
  , websocketV2Handler
  , schedulerIteration
  , server
  )where

import           Servant
import qualified Servant.API
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
import           Data.OpEnergy.API.V1.Positive(Positive, naturalFromPositive)
import qualified OpEnergy.Server.GitCommitHash as Server
import qualified OpEnergy.Server.V1.Metrics as Metrics( MetricsState(..))
import           OpEnergy.Server.V1.Class (AppT, AppM, runLogging, State(..))
import           OpEnergy.Server.V1.BlockHeadersService(syncBlockHeaders, getBlockHeaderByHash, getBlockHeaderByHeight, mgetBlockHeaderByHeight)
import           OpEnergy.Server.V1.WebSocketService(webSocketConnection)
import           OpEnergy.Server.V1.BlockSpanService(getBlockSpanListByRange, getBlockSpanList)
import           OpEnergy.Server.V1.StatisticsService(getTheoreticalActualMTPPercents)
import           Data.Text.Show(tshow)

import           Prometheus(MonadMonitor)
import qualified Prometheus as P
import           Data.OpEnergy.API.V1.Error(throwJSON)

-- | DEPRECATED wrapper for legacy blockspanlist endpoint
getBlockSpanListDeprecated :: BlockHeight -> Positive Int -> Positive Int -> AppM (Headers '[Servant.API.Header "Deprecation" String, Servant.API.Header "Sunset" String, Servant.API.Header "Link" String] [BlockSpan])
getBlockSpanListDeprecated startHeight spansize numberOfSpans = do
  result <- OpEnergy.Server.V1.BlockSpanService.getBlockSpanList startHeight spansize numberOfSpans
  return $ addDeprecationHeaders result

-- | Helper to convert Positive Int to proper value, using default spansize of 24
defaultSpansize :: Maybe (Positive Int) -> Positive Int
defaultSpansize (Just s) = s
defaultSpansize Nothing = 24

-- | Helper to add deprecation headers to responses
addDeprecationHeaders :: a -> Headers '[Servant.API.Header "Deprecation" String, Servant.API.Header "Sunset" String, Servant.API.Header "Link" String] a
addDeprecationHeaders value = addHeader "true" $ addHeader "2024-12-31" $ addHeader "</api/v2/blockspans/blockspans>; rel=\"successor-version\"" value


websocketHandler :: ServerT WebSocketAPI (AppT Handler)
websocketHandler = OpEnergy.Server.V1.WebSocketService.webSocketConnection

-- | V2 WebSocket handler - same functionality as V1 but at /blockspans/ws
websocketV2Handler :: ServerT WebSocketV2API (AppT Handler)
websocketV2Handler = OpEnergy.Server.V1.WebSocketService.webSocketConnection

-- | here goes implementation of OpEnergy API, which should match Data.OpEnergy.API.V2.V2API
server:: ServerT V2API (AppT Handler)
server = OpEnergy.Server.V1.BlockHeadersService.getBlockHeaderByHash
    :<|> OpEnergy.Server.V1.BlockHeadersService.getBlockHeaderByHeight
    :<|> getBlocksByBlockSpanDeprecated
    :<|> getBlocksWithNbdrByBlockSpanDeprecated
    :<|> getBlocksWithHashrateByBlockSpanDeprecated
    :<|> getBlockSpanListDeprecated
    :<|> oeGitHashGet
    :<|> getSingleBlockspan  -- New V2 endpoint
    :<|> getMultipleBlockspans  -- New V2 endpoint

-- | one iteration that called from scheduler thread
schedulerIteration :: (MonadIO m, MonadMonitor m) => AppT m ()
schedulerIteration = OpEnergy.Server.V1.BlockHeadersService.syncBlockHeaders

-- | DEPRECATED wrapper for legacy endpoint
getBlocksByBlockSpanDeprecated
  :: BlockHeight
  -> Positive Int
  -> Maybe (Positive Int)
  -> Maybe Bool
  -> Maybe Bool
  -> AppM (Headers '[Servant.API.Header "Deprecation" String, Servant.API.Header "Sunset" String, Servant.API.Header "Link" String] [BlockSpanHeadersNbdrHashrate])
getBlocksByBlockSpanDeprecated startHeight spansize mNumberOfSpans mNbdr mHashrate = do
  result <- getBlocksByBlockSpan startHeight spansize mNumberOfSpans mNbdr mHashrate
  return $ addDeprecationHeaders result

getBlocksByBlockSpan
  :: BlockHeight
  -> Positive Int
  -> Maybe (Positive Int)
  -> Maybe Bool
  -> Maybe Bool
  -> AppM [BlockSpanHeadersNbdrHashrate]
getBlocksByBlockSpan startHeight span mNumberOfSpan mNbdr mHashrate = do
  State{ metrics = Metrics.MetricsState{ getBlocksByBlockSpan = getBlocksByBlockSpan}
       , currentTip = currentTipV
       } <- ask
  P.observeDuration getBlocksByBlockSpan $ do
    mCurrentTip <- liftIO $! TVar.readTVarIO currentTipV
    let withNbdr = case mNbdr of
          Just some -> some
          _ -> False
        withHashrate = case mHashrate of
          Just some -> some
          _ -> False
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

-- | DEPRECATED wrapper for legacy endpoint
getBlocksWithNbdrByBlockSpanDeprecated
  :: BlockHeight
  -> Positive Int
  -> Maybe (Positive Int)
  -> AppM (Headers '[Servant.API.Header "Deprecation" String, Servant.API.Header "Sunset" String, Servant.API.Header "Link" String] [BlockSpanHeadersNbdr])
getBlocksWithNbdrByBlockSpanDeprecated startHeight spansize mNumberOfSpans = do
  result <- getBlocksWithNbdrByBlockSpan startHeight spansize mNumberOfSpans
  return $ addDeprecationHeaders result

getBlocksWithNbdrByBlockSpan
  :: BlockHeight
  -> Positive Int
  -> Maybe (Positive Int)
  -> AppM [BlockSpanHeadersNbdr]
getBlocksWithNbdrByBlockSpan startHeight span mNumberOfSpans = do
  State{ metrics = Metrics.MetricsState{ getBlocksWithNbdrByBlockSpan = getBlocksWithNbdrByBlockSpan} } <- ask
  P.observeDuration getBlocksWithNbdrByBlockSpan $ do
    blockSpansBlocks <- getBlocksByBlockSpan startHeight span mNumberOfSpans (Just True) Nothing
    return $! map toBlockSpanHeadersNbdr blockSpansBlocks
  where
    toBlockSpanHeadersNbdr (BlockSpanHeadersNbdrHashrate {..}) = BlockSpanHeadersNbdr
      { startBlock = startBlock
      , endBlock = endBlock
      , Data.OpEnergy.API.V1.nbdr = fromJust $! nbdr
      }

-- | DEPRECATED wrapper for legacy endpoint
getBlocksWithHashrateByBlockSpanDeprecated
  :: BlockHeight
  -> Positive Int
  -> Maybe (Positive Int)
  -> AppM (Headers '[Servant.API.Header "Deprecation" String, Servant.API.Header "Sunset" String, Servant.API.Header "Link" String] [BlockSpanHeadersHashrate])
getBlocksWithHashrateByBlockSpanDeprecated startHeight spansize mNumberOfSpans = do
  result <- getBlocksWithHashrateByBlockSpan startHeight spansize mNumberOfSpans
  return $ addDeprecationHeaders result

getBlocksWithHashrateByBlockSpan
  :: BlockHeight
  -> Positive Int
  -> Maybe (Positive Int)
  -> AppM [BlockSpanHeadersHashrate]
getBlocksWithHashrateByBlockSpan startHeight span mNumberOfSpans = do
  State{ metrics = Metrics.MetricsState{ getBlocksWithHashrateByBlockSpan = getBlocksWithHashrateByBlockSpan} } <- ask
  P.observeDuration getBlocksWithHashrateByBlockSpan $ do
    blockSpansBlocks <- getBlocksByBlockSpan startHeight span mNumberOfSpans Nothing (Just True)
    return $! map toBlockSpanHeadersHashrate blockSpansBlocks
  where
    toBlockSpanHeadersHashrate (BlockSpanHeadersNbdrHashrate {..}) = BlockSpanHeadersHashrate
      { startBlock = startBlock
      , endBlock = endBlock
      , Data.OpEnergy.API.V1.hashrate = fromJust hashrate
      }

-- returns just commit hash, provided by build system
oeGitHashGet :: AppT Handler GitHashResponse
oeGitHashGet = return $ GitHashResponse
  { gitCommitHash = Server.gitCommitHash
  }

-- | V2 endpoint: Get single blockspan by end height (document spec: /blockspans/blockspan/{blockHeight}/)
getSingleBlockspan :: BlockHeight -> Maybe (Positive Int) -> Maybe Bool -> AppM BlockSpanV2
getSingleBlockspan blockHeight mSpansize _mWithHeaderInfos = do
  let spansize = defaultSpansize mSpansize
      startHeight = blockHeight - naturalFromPositive spansize
  mstart <- OpEnergy.Server.V1.BlockHeadersService.mgetBlockHeaderByHeight startHeight
  mend <- OpEnergy.Server.V1.BlockHeadersService.mgetBlockHeaderByHeight blockHeight
  case (mstart, mend) of
    (Just start, Just end) -> do
      let nbdrValue = getTheoreticalActualMTPPercents start end
          hashrateValue = (blockHeaderChainwork end - blockHeaderChainwork start) `div` (fromIntegral (blockHeaderMediantime end - blockHeaderMediantime start))
      return $ BlockSpanV2
        { startBlock = start
        , endBlock = end
        , nbdr = nbdrValue
        , hashrate = hashrateValue
        }
    _ -> do
      let err = "ERROR: getSingleBlockspan: failed to get block headers for blockspan {" <> tshow startHeight <> ", " <> tshow blockHeight <> "}"
      runLogging $ $(logError) err
      throwJSON err400 err

-- | V2 endpoint: Get multiple blockspans (document spec: /blockspans/blockspans/{startBlockHeight}/{numberOfSpans})
getMultipleBlockspans 
  :: BlockHeight           -- startBlockHeight (required)
  -> Positive Int          -- numberOfSpans (required)
  -> Maybe (Positive Int)  -- spansize (optional, defaults to 24)
  -> Maybe Bool            -- withHeaders (optional)
  -> AppM [BlockSpanV2]
getMultipleBlockspans startBlockHeight numberOfSpans mSpansize _mWithHeaders = do
  State{ currentTip = currentTipV } <- ask
  mCurrentTip <- liftIO $! TVar.readTVarIO currentTipV
  
  let spansize = defaultSpansize mSpansize
  
  case mCurrentTip of
    Nothing -> do
      let err = "ERROR: getMultipleBlockspans: no current tip discovered yet"
      runLogging $ $(logError) err
      throwJSON err400 err
    Just _currentTip -> do
      -- Get blockspans using existing service
      spans <- OpEnergy.Server.V1.BlockSpanService.getBlockSpanList startBlockHeight spansize numberOfSpans
      
      -- Convert to V2 format with both nbdr and hashrate (always included)
      forM spans $ \(BlockSpan startHeight endHeight) -> do
        mstart <- OpEnergy.Server.V1.BlockHeadersService.mgetBlockHeaderByHeight startHeight
        mend <- OpEnergy.Server.V1.BlockHeadersService.mgetBlockHeaderByHeight endHeight
        case (mstart, mend) of
          (Just start, Just end) -> do
            let nbdrValue = getTheoreticalActualMTPPercents start end
                hashrateValue = (blockHeaderChainwork end - blockHeaderChainwork start) `div` (fromIntegral (blockHeaderMediantime end - blockHeaderMediantime start))
            return $ BlockSpanV2
              { startBlock = start
              , endBlock = end
              , nbdr = nbdrValue
              , hashrate = hashrateValue
              }
          _ -> do
            let err = "ERROR: getMultipleBlockspans: failed to get block headers for blockspan {" <> tshow startHeight <> ", " <> tshow endHeight <> "}"
            runLogging $ $(logError) err
            throwJSON err400 err

