{-- | This module provides service responsible for synchronizing BlockHeaders DB with Bitcoin node
 -}
{-# LANGUAGE TemplateHaskell     #-}
module OpEnergy.Server.V1.BlockHeadersService
  ( syncBlockHeaders
  , getBlockHeaderByHash
  , getBlockHeaderByHeight
  , mgetBlockHeaderByHeight
  , loadDBState
  , cacheBlockHeadersFromDB
  ) where

import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import           Data.Maybe(fromJust)
import           Data.Pool(Pool)
import           Servant.API (BasicAuthData(..))
import           Servant (err404, errBody, throwError)
import           Servant.Client.JsonRpc
import           Control.Monad (foldM)
import           Control.Monad.Logger (logDebug, logInfo)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.Encoding as Text
import           Data.Text.Show (tshow)

import           Database.Persist.Postgresql

import           Data.Bitcoin.API as Bitcoin
import           Data.Bitcoin.BlockStats as BlockStats
import           Data.Bitcoin.BlockInfo as BlockInfo
import           Data.OpEnergy.API.V1.Block
import           OpEnergy.Server.V1.Config
import           OpEnergy.Server.V1.Class (runLogging, AppT, AppM, State(..))
import           OpEnergy.Server.V1.Metrics(MetricsState(..))
import qualified OpEnergy.Server.V1.BlockHeadersService.Vector.Service as Cache
import           Prometheus(MonadMonitor)
import qualified Prometheus as P


getBlockHeaderByHash :: BlockHash -> AppM BlockHeader
getBlockHeaderByHash hash = do
  mheader <- mgetBlockHeaderByHash hash
  case mheader of
    Just header -> return header
    Nothing-> throwError err404 {errBody = "ERROR: getBlockHeaderByHash: failed to find block header by given hash"}


-- | returns BlockHeader by given hash
-- Complexity:
-- - O(log n) {cache Hash -> Height lookup} + O(log n) { Height -> Header cache lookup} - in case if hash exists in cache
-- - 2 * O(log n) {cache lookup } + DB lookup + 2 * O(log n) {cache insertion} - in case if hash is not cached yet
mgetBlockHeaderByHash :: (MonadIO m, MonadMonitor m) => BlockHash -> AppT m (Maybe BlockHeader)
mgetBlockHeaderByHash hash = do
  State{ metrics = MetricsState { mgetBlockHeaderByHashH = mgetBlockHeaderByHashH
                                }
       } <- ask
  P.observeDuration mgetBlockHeaderByHashH $ Cache.lookupByHash hash

-- | returns BlockHeader by given height. See mgetBlockHeaderByHeight for reference
getBlockHeaderByHeight :: BlockHeight -> AppM BlockHeader
getBlockHeaderByHeight height = do
  mheader <- mgetBlockHeaderByHeight height
  case mheader of
    Just header -> return header
    _ -> throwError err404 {errBody = "ERROR: getBlockHeaderByHeight: failed to find block header by given height"}

-- | returns Just BlockHeader by given height or Nothing if there no block with given height
-- - O(log n) - in case if block with given height is in Height -> BlockHeader cache;
-- - O(log n) {cache lookup} + O(DB lookup) + 2 * O(log n) {cache insertion} in case if no such block header in the cache yet
mgetBlockHeaderByHeight :: (MonadIO m, MonadMonitor m) => BlockHeight -> AppT m (Maybe BlockHeader)
mgetBlockHeaderByHeight height = do
  State{ metrics = MetricsState { mgetBlockHeaderByHeightH = mgetBlockHeaderByHeightH
                                }
       } <- ask
  P.observeDuration mgetBlockHeaderByHeightH $ Cache.lookupByHeight height

-- | returns the newest confirmed BlockHeader or Nothing if there are no blocks found yet
mgetLastBlockHeader :: Pool SqlBackend-> IO (Maybe (Entity BlockHeader))
mgetLastBlockHeader pool = flip runSqlPersistMPool pool $ selectFirst ([] :: [Filter BlockHeader]) [ Desc BlockHeaderHeight ]

-- | performs read from DB in order to set State.currentHeightTip
loadDBState :: (MonadIO m, MonadMonitor m) => AppT m ()
loadDBState = do
  State{ blockHeadersDBPool = pool
       , currentTip = currentTipV
       , metrics = MetricsState {loadDBStateH = loadDBStateH}
       } <- ask
  P.observeDuration loadDBStateH $ do
    mlast <- liftIO $ mgetLastBlockHeader pool
    case mlast of
      Nothing-> return () -- do nothing
      Just (Entity _ header) -> do
        liftIO $ STM.atomically $ TVar.writeTVar currentTipV (Just header)
        runLogging $ $(logInfo) ("current confirmed height tip " <> tshow (blockHeaderHeight header))
        cacheBlockHeadersFromDB (blockHeaderHeight header)
        runLogging $ $(logInfo) "cached block headers"

-- | this procedure ensures that BlockHeaders table is in sync with block chain
syncBlockHeaders :: (MonadIO m, MonadMonitor m) => AppT m ()
syncBlockHeaders = do
  State{ metrics = MetricsState{ syncBlockHeadersH = syncBlockHeadersH}} <- ask
  runLogging $ $(logDebug) "syncBlockHeaders"
  P.observeDuration syncBlockHeadersH $ do
    mstartSyncHeightFromTo <- mgetHeightToStartSyncFromTo
    case mstartSyncHeightFromTo of
      Nothing-> return () -- do nothing if sync is not needed
      Just (startSyncHeightFrom, startSyncHeightTo) -> do
        newestConfirmedBlockHeader <- performSyncFromTo startSyncHeightFrom startSyncHeightTo
        runLogging $ $(logDebug) $ "new latest confirmed block height " <> tshow startSyncHeightTo
        updateLatestConfirmedHeightTip newestConfirmedBlockHeader -- cache newest header
  where
    updateLatestConfirmedHeightTip header = do
      State{ currentTip = currentTipV } <- ask
      liftIO $ STM.atomically $ TVar.writeTVar currentTipV (Just header)

    -- | queries bitcoin node and compares with latest witnessed block
    mgetHeightToStartSyncFromTo :: MonadIO m => AppT m (Maybe (BlockHeight, BlockHeight))
    mgetHeightToStartSyncFromTo = do
      State{ config = config, currentTip = currentTipV, metrics = MetricsState{ btcGetBlockchainInfoH = btcGetBlockchainInfoH}} <- ask
      mcurrentConfirmedTip <- liftIO $ TVar.readTVarIO currentTipV
      let userPass = BasicAuthData (Text.encodeUtf8 $ configBTCUser config) (Text.encodeUtf8 $ configBTCPassword config)
      eblockchainInfo <- liftIO $ P.observeDuration btcGetBlockchainInfoH $ Bitcoin.withBitcoin ( configBTCURL config) (getBlockchainInfo userPass [])
      case eblockchainInfo of
        (Result _ blockchainInfo ) -> do
          let newUnconfirmedHeightTip = Bitcoin.blocks blockchainInfo
          runLogging $ $(logDebug) ( "current unconfirmed height tip is " <> tshow newUnconfirmedHeightTip)
          case mcurrentConfirmedTip of
            Just currentConfirmedTip
              | blockHeaderHeight currentConfirmedTip + (configBlocksToConfirm config) >= newUnconfirmedHeightTip -> return Nothing
            _ | newUnconfirmedHeightTip < (configBlocksToConfirm config) -> return Nothing -- do nothing, if there are no confirmed blocks yet
            _ -> do -- there are some confirmed blocks, that we are not aware of, need to sync DB
              let confirmedHeightFrom =
                    case mcurrentConfirmedTip of
                      Nothing -> 0 -- no previously confirmed tip, start with 0
                      Just currentConfirmedTip -> (blockHeaderHeight currentConfirmedTip + 1) -- start with the next unconfirmed tip
                  confirmedHeightTo = newUnconfirmedHeightTip - (configBlocksToConfirm config)
              return $ Just (confirmedHeightFrom, confirmedHeightTo)
        some -> error ( "syncBlockHeaders: getBlockchainInfo error: " ++ show some)

    performSyncFromTo confirmedHeightFrom confirmedHeightTo = do
      Cache.ensureCapacity confirmedHeightTo
      mlastBH <- foldM  ( \_ height -> do -- fold over all blocks returning the last block header
          runLogging $ $(logDebug) $ "height " <> tshow height
          (bi, reward) <- getBlockInfos height
          let bh = blockHeaderFromBlockInfos bi reward
          persistBlockHeader bh
          Cache.maybeInsert bh
          return $! Just bh
        )
        Nothing
        [ confirmedHeightFrom .. confirmedHeightTo ]
      return $! fromJust mlastBH
      where
        persistBlockHeader :: MonadIO m => BlockHeader -> AppT m ()
        persistBlockHeader header = do
          State{ blockHeadersDBPool = pool
               , metrics = MetricsState { blockHeaderDBInsertH = blockHeaderDBInsertH
                                        }
               } <- ask
          _ <- liftIO $ P.observeDuration blockHeaderDBInsertH $ flip runSqlPersistMPool pool $ insert header
          return ()

        getBlockInfos height = do
          State{ config = config
               , metrics = MetricsState { btcGetBlockHashH = btcGetBlockHashH
                                        , btcGetBlockH = btcGetBlockH
                                        , btcGetBlockStatsH = btcGetBlockStatsH
                                        }
               } <- ask
          let userPass = BasicAuthData (Text.encodeUtf8 $ configBTCUser config) (Text.encodeUtf8 $ configBTCPassword config)
          liftIO $ do
            Result _ hash <- P.observeDuration btcGetBlockHashH $ Bitcoin.withBitcoin ( configBTCURL config) $ getBlockHash userPass [height]
            Result _ bi <- P.observeDuration btcGetBlockH $ Bitcoin.withBitcoin ( configBTCURL config) $ getBlock userPass [ hash ]
            if height == 0
              then return (bi, 5000000000 {- default subsidy-} )
              else do
                Result _ bs <- P.observeDuration btcGetBlockStatsH $ Bitcoin.withBitcoin ( configBTCURL config) $ getBlockStats userPass [height]
                return (bi, BlockStats.totalfee bs + BlockStats.subsidy bs)

        blockHeaderFromBlockInfos bi reward = BlockHeader
          { blockHeaderHash = BlockInfo.hash bi
          , blockHeaderPreviousblockhash = BlockInfo.previousblockhash bi
          , blockHeaderHeight = BlockInfo.height bi
          , blockHeaderVersion = BlockInfo.version bi
          , blockHeaderTimestamp = BlockInfo.time bi
          , blockHeaderBits = BlockInfo.bits bi
          , blockHeaderNonce = BlockInfo.nonce bi
          , blockHeaderDifficulty = BlockInfo.difficulty bi
          , blockHeaderMerkle_root = BlockInfo.merkleroot bi
          , blockHeaderTx_count = BlockInfo.nTx bi
          , blockHeaderSize = BlockInfo.size bi
          , blockHeaderWeight = BlockInfo.weight bi
          , blockHeaderChainwork = BlockInfo.chainwork bi
          , blockHeaderMediantime = BlockInfo.mediantime bi
          , blockHeaderReward = reward
          }

cacheBlockHeadersFromDB
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockHeight
    -- ^ block height end
  -> AppT m ()
cacheBlockHeadersFromDB end = do
  runLogging $ $(logDebug) $! "caching block headers [ 0 .. " <> tshow end <> " ]"
  State{ blockHeadersDBPool = pool
       , metrics = MetricsState { blockHeaderCacheFromDBLookup = blockHeaderCacheFromDBLookup }
       } <- ask
  runLogging $ $(logDebug) $! "DB query"
  -- TODO: here is a possible optimization: persistent do not support chunk/lazy fetching, so this query will load all the records in the RAM. this can be walkarounded with pagination+conduits.
  headers <- liftIO $ P.observeDuration blockHeaderCacheFromDBLookup $ flip runSqlPersistMPool pool $ selectList [ BlockHeaderHeight >=. 0, BlockHeaderHeight <=. end ] [ Asc BlockHeaderHeight]
  runLogging $ $(logDebug) $! "DB query done"
  Cache.ensureCapacity end
  runLogging $ $(logDebug) $! "inserting block headers into the cache"
  Cache.maybeInsertMany $ map (\(Entity _ header)-> header) headers
  runLogging $ $(logDebug) $! "done inserting"
