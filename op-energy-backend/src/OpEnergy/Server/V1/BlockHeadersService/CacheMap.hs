{-- | This module provides service responsible for synchronizing BlockHeaders DB with Bitcoin node
 -}
{-# LANGUAGE TemplateHaskell     #-}
module OpEnergy.Server.V1.BlockHeadersService.Cache
  ( insert
  , lookupByHeight
  , lookupByHash
  , init
  ) where

import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import           Data.Pool(Pool)
import qualified Data.Map.Strict as Map
import           Servant.API (BasicAuthData(..))
import           Servant (err400, Handler)
import           Servant.Client.JsonRpc
import           Control.Monad (forM_)
import           Control.Monad.Logger (logDebug, logInfo)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.Encoding as Text
import           Data.Text.Show (tshow)
import           Data.Vector(Vector)
import qualified Data.Vector as V

import           Database.Persist.Postgresql

import           Data.Bitcoin.API as Bitcoin
import           Data.Bitcoin.BlockStats as BlockStats
import           Data.Bitcoin.BlockInfo as BlockInfo
import           Data.OpEnergy.API.V1.Block
import           OpEnergy.Server.V1.Config
import           OpEnergy.Server.V1.Class (runLogging, runAppT, AppT, AppM, State(..))
import           OpEnergy.Server.V1.Metrics(MetricsState(..))
import           Prometheus(MonadMonitor)
import qualified Prometheus as P


instance MonadMonitor Handler where
  doIO = liftIO

newtype BlockHeaderCache = BlockHeaderCache
  { heightCache :: (Vector (Vector Blockheader))
  , hashCache :: Map BlockHash BlockHeight
  , writeLock :: MVar ()
    -- ^ writer should perform takeMVar/putMVar in order to sync
  }

getBlockHeaderByHash :: BlockHash -> AppM BlockHeader
getBlockHeaderByHash hash = do
  mheader <- mgetBlockHeaderByHash hash
  case mheader of
    Nothing-> throwJSON err400 ("no block found with given hash"::Text)
    Just header -> return header


-- | returns BlockHeader by given hash
-- Complexity:
-- - O(log n) {cache Hash -> Height lookup} + O(log n) { Height -> Header cache lookup} - in case if hash exists in cache
-- - 2 * O(log n) {cache lookup } + DB lookup + 2 * O(log n) {cache insertion} - in case if hash is not cached yet
mgetBlockHeaderByHash :: (MonadIO m, MonadMonitor m) => BlockHash -> AppT m (Maybe BlockHeader)
mgetBlockHeaderByHash hash = do
  state@State{ blockHeadersDBPool = pool
             , blockHeadersHashCache = blockHeadersHashCacheV
             , blockHeadersHeightCache = blockHeadersHeightCacheV
             , metrics = MetricsState { mgetBlockHeaderByHashH = mgetBlockHeaderByHashH
                                      , mgetBlockHeaderByHashCacheH = mgetBlockHeaderByHashCacheH
                                      , mgetBlockHeaderByHashCacheHit = mgetBlockHeaderByHashCacheHit
                                      , mgetBlockHeaderByHashCacheMiss = mgetBlockHeaderByHashCacheMiss
                                      , mgetBlockHeaderByHashCacheInsert = mgetBlockHeaderByHashCacheInsert
                                      , mgetBlockHeaderByHeightCacheInsert = mgetBlockHeaderByHeightCacheInsert
                                      , mgetBlockHeaderByHashCacheDBLookup = mgetBlockHeaderByHashCacheDBLookup
                                      }
             } <- ask
  P.observeDuration mgetBlockHeaderByHashH $ do
    blockHeadersHashCache <- liftIO $ TVar.readTVarIO blockHeadersHashCacheV
    mCachedHash <- P.observeDuration mgetBlockHeaderByHashCacheH $ return $! Map.lookup hash blockHeadersHashCache
    case mCachedHash of -- check cache first
      Just height -> do
        runLogging $ $(logDebug) $ "hash " <> tshow hash <> " is a height " <> tshow height <> " and had been found in the cache"
        P.incCounter mgetBlockHeaderByHashCacheHit
        runAppT state $ mgetBlockHeaderByHeight height
      Nothing -> do -- there is no header in cache
        P.incCounter mgetBlockHeaderByHashCacheMiss
        mheader <- liftIO $ P.observeDuration mgetBlockHeaderByHashCacheDBLookup $ flip runSqlPersistMPool pool $ selectFirst [ BlockHeaderHash ==. hash ] []
        case mheader of
          Nothing-> return Nothing
          Just (Entity _ header) -> do
            let height = blockHeaderHeight header
            liftIO $ P.observeDuration mgetBlockHeaderByHeightCacheInsert $ STM.atomically $
              TVar.modifyTVar blockHeadersHeightCacheV $ \cache-> Map.insert height header cache -- update cache
            liftIO $ P.observeDuration mgetBlockHeaderByHashCacheInsert $ STM.atomically $
              TVar.modifyTVar blockHeadersHashCacheV $ \cache-> Map.insert hash height cache -- update cache
            runLogging $ $(logDebug) $ "header with height " <> tshow height <> " and hash " <> tshow hash <> " inserted into the cache"
            return (Just header)

-- | returns BlockHeader by given height. See mgetBlockHeaderByHeight for reference
getBlockHeaderByHeight :: BlockHeight -> AppM BlockHeader
getBlockHeaderByHeight height = do
  mheader <- mgetBlockHeaderByHeight height
  case mheader of
    Just header -> return header
    _ -> throwJSON err400 ("no block found with given height" ::Text)

-- | returns Just BlockHeader by given height or Nothing if there no block with given height
-- - O(log n) - in case if block with given height is in Height -> BlockHeader cache;
-- - O(log n) {cache lookup} + O(DB lookup) + 2 * O(log n) {cache insertion} in case if no such block header in the cache yet
mgetBlockHeaderByHeight :: (MonadIO m, MonadMonitor m) => BlockHeight -> AppT m (Maybe BlockHeader)
mgetBlockHeaderByHeight height = do
  State{ blockHeadersDBPool = pool
       , currentTip = currentTipV
       , blockHeadersHashCache = blockHeadersHashCacheV
       , blockHeadersHeightCache = blockHeadersHeightCacheV
       , metrics = MetricsState { mgetBlockHeaderByHeightH = mgetBlockHeaderByHeightH
                                , mgetBlockHeaderByHeightCacheH = mgetBlockHeaderByHeightCacheH
                                , mgetBlockHeaderByHeightCacheHit = mgetBlockHeaderByHeightCacheHit
                                , mgetBlockHeaderByHeightCacheMiss = mgetBlockHeaderByHeightCacheMiss
                                , mgetBlockHeaderByHeightCacheInsert = mgetBlockHeaderByHeightCacheInsert
                                , mgetBlockHeaderByHashCacheInsert = mgetBlockHeaderByHashCacheInsert
                                , mgetBlockHeaderByHeightCacheDBLookup = mgetBlockHeaderByHeightCacheDBLookup
                                }
       } <- ask
  P.observeDuration mgetBlockHeaderByHeightH $ do
    mcurrentTip <- liftIO $ TVar.readTVarIO currentTipV
    blockHeadersHeightCache <- liftIO $ TVar.readTVarIO blockHeadersHeightCacheV
    case mcurrentTip of
      Just currentTip
        | height == blockHeaderHeight currentTip -> return (Just currentTip) -- the fastest case is that caller is looking for a current tip
        | height < blockHeaderHeight currentTip -> do -- requested height is confirmed one
            mCachedHeader <- P.observeDuration mgetBlockHeaderByHeightCacheH $ return $! Map.lookup height blockHeadersHeightCache
            case mCachedHeader of -- check cache first
              Just header -> do
                P.incCounter mgetBlockHeaderByHeightCacheHit
                runLogging $ $(logDebug) $ "header with height " <> tshow height <> " found in the cache"
                return (Just header)
              Nothing -> do -- there is no header in cache
                P.incCounter mgetBlockHeaderByHeightCacheMiss
                mheader <- liftIO $ P.observeDuration mgetBlockHeaderByHeightCacheDBLookup $ flip runSqlPersistMPool pool $ selectFirst [ BlockHeaderHeight ==. height ] []
                case mheader of
                  Nothing-> return Nothing
                  Just (Entity _ header) -> do
                    liftIO $ P.observeDuration mgetBlockHeaderByHeightCacheInsert $
                      STM.atomically $ TVar.modifyTVar blockHeadersHeightCacheV $ \cache-> Map.insert height header cache -- update cache
                    liftIO $ P.observeDuration mgetBlockHeaderByHashCacheInsert $
                      STM.atomically $ TVar.modifyTVar blockHeadersHashCacheV $ \cache-> Map.insert (blockHeaderHash header) height cache -- update cache
                    runLogging $ $(logDebug) $ "header with height " <> tshow height <> " inserted in the cache"
                    return (Just header)
      _ -> return Nothing

-- | returns the newest confirmed BlockHeader or Nothing if there are no blocks found yet
mgetLastBlockHeader :: Pool SqlBackend-> IO (Maybe (Entity BlockHeader))
mgetLastBlockHeader pool = flip runSqlPersistMPool pool $ selectFirst ([] :: [Filter BlockHeader]) [ Desc BlockHeaderHeight ]

-- | performs read from DB in order to set State.currentHeightTip
loadDBState :: MonadIO m => AppT m ()
loadDBState = do
  State{ blockHeadersDBPool = pool, currentTip = currentTipV } <- ask
  mlast <- liftIO $ mgetLastBlockHeader pool
  case mlast of
    Nothing-> return () -- do nothing
    Just (Entity _ header) -> do
      liftIO $ STM.atomically $ TVar.writeTVar currentTipV (Just header)
      runLogging $ $(logInfo) ("current confirmed height tip " <> tshow (blockHeaderHeight header))

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
      forM_ [ confirmedHeightFrom .. confirmedHeightTo ] $ \height -> do
        runLogging $ $(logDebug) $ "height " <> tshow height
        (bi, reward) <- getBlockInfos height
        let bh = blockHeaderFromBlockInfos bi reward
        persistBlockHeader bh
      (bi, reward) <- getBlockInfos confirmedHeightTo -- now we need to return the last confirmed block to the caller
      return $ blockHeaderFromBlockInfos bi reward
      where
        persistBlockHeader :: MonadIO m => BlockHeader -> AppT m ()
        persistBlockHeader header = do
          State{ blockHeadersDBPool = pool
               , metrics = MetricsState { blockHeaderDBInsertH = blockHeaderDBInsertH}
               } <- ask
          _ <- liftIO $ P.observeDuration blockHeaderDBInsertH $ flip runSqlPersistMPool pool $ insert header
          return ()

        getBlockInfos height = do
          State{ config = config } <- ask
          let userPass = BasicAuthData (Text.encodeUtf8 $ configBTCUser config) (Text.encodeUtf8 $ configBTCPassword config)
          liftIO $ Bitcoin.withBitcoin ( configBTCURL config) $ do
            Result _ hash <- getBlockHash userPass [height]
            Result _ bi <- getBlock userPass [ hash ]
            if height == 0
              then return (bi, 5000000000 {- default subsidy-} )
              else do
                Result _ bs <- getBlockStats userPass [height]
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
    -- ^ block height start
  -> BlockHeight
    -- ^ block height end
  -> AppT m ()
cacheBlockHeadersFromDB start end = do
  State{ blockHeadersDBPool = pool
       , blockHeadersHashCache = blockHeadersHashCacheV
       , blockHeadersHeightCache = blockHeadersHeightCacheV
       , metrics = MetricsState { mgetBlockHeaderByHeightCacheInsert = mgetBlockHeaderByHeightCacheInsert
                                , mgetBlockHeaderByHashCacheInsert = mgetBlockHeaderByHashCacheInsert
                                , mgetBlockHeaderByHeightCacheDBLookup = mgetBlockHeaderByHeightCacheDBLookup
                                }
       } <- ask
  headers <- liftIO $ P.observeDuration mgetBlockHeaderByHeightCacheDBLookup $ flip runSqlPersistMPool pool $ selectList [ BlockHeaderHeight >=. start, BlockHeaderHeight <=. end ] [ Asc BlockHeaderHeight]
  forM_ headers $ \(Entity _ header) -> do
    liftIO $ P.observeDuration mgetBlockHeaderByHeightCacheInsert $
      STM.atomically $ TVar.modifyTVar blockHeadersHeightCacheV $ \cache-> Map.insert (blockHeaderHeight header) header cache -- update cache
    liftIO $ P.observeDuration mgetBlockHeaderByHashCacheInsert $
      STM.atomically $ TVar.modifyTVar blockHeadersHashCacheV $ \cache-> Map.insert (blockHeaderHash header) (blockHeaderHeight header) cache -- update cache
