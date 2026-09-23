{-- | This module provides service responsible for synchronizing BlockHeaders DB with Bitcoin node
 -}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
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
import           Servant (err400 )
import           Servant.Client(ClientEnv)
import           Servant.Client.JsonRpc
import           Control.Monad (foldM, when)
import           Control.Monad.Logger (logDebug, logInfo, logError)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.Trans.Except ( ExceptT(..))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text( Text)
import qualified Data.Text.Encoding as Text
import           Data.Text.Show (tshow)
import           Data.Word

import           Database.Persist.Postgresql

import           Data.Bitcoin.API as Bitcoin
import           Data.Bitcoin.BlockStats as BlockStats
import           Data.Bitcoin.BlockInfo as BlockInfo
import           Data.OpEnergy.API.V1.Block
import           OpEnergy.Server.V1.Config
import           OpEnergy.Server.V1.Class (profile, runLogging, AppT, AppM, State(..))
import           OpEnergy.Server.V1.Metrics(MetricsState(..))
import qualified OpEnergy.Server.V1.BlockHeadersService.Vector.Service as Cache
import           OpEnergy.Server.Common
                   ( runExceptPrefixT, exceptTMaybeT
                   )
import           Prometheus(MonadMonitor)
import qualified Prometheus as P
import           Data.OpEnergy.API.V1.Error(throwJSON)


getBlockHeaderByHash :: BlockHash -> AppM BlockHeader
getBlockHeaderByHash hash = do
  mheader <- mgetBlockHeaderByHash hash
  case mheader of
    Just header -> return header
    Nothing-> throwJSON err400 ("ERROR: getBlockHeaderByHash: failed to find block header by given hash"::Text)


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
    _ -> throwJSON err400 ("ERROR: getBlockHeaderByHeight: failed to find block header by given height"::Text)

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

-- | performs read from DB in order to set State.currentHeightTip,
-- and fetches the unconfirmed chain tip from bitcoind so the
-- websocket's @oe-latest-unconfirmed-block@ is populated from the
-- first message.
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

-- | this procedure ensures that BlockHeaders table is in sync with block chain,
-- and keeps the unconfirmed tip header up to date for the websocket.
syncBlockHeaders :: (MonadIO m, MonadMonitor m) => AppT m ()
syncBlockHeaders =
    let
      name = "syncBlockHeaders"
    in profile name $ do
  State{ config = config
       , currentTip = currentTipV
       , unconfirmedTip = unconfirmedTipV
       } <- ask
  eret <- runExceptPrefixT name $ do
        let userPass = BasicAuthData (Text.encodeUtf8 $ configBTCUser config) (Text.encodeUtf8 $ configBTCPassword config)
        clientEnv <- liftIO $! Bitcoin.mkEnv (configBTCURL config)
        blockchainInfo <- ExceptT $! liftIO $! Bitcoin.withBitcoinEnv clientEnv $! do
          Result _ blockchainInfo <- getBlockchainInfo userPass []
          return blockchainInfo
        let newUnconfirmedHeightTip = Bitcoin.blocks blockchainInfo

        (mcurrentUnconfirmedTip, mcurrentConfirmedTip) <- liftIO $ STM.atomically $ (,)
          <$> TVar.readTVar unconfirmedTipV
          <*> TVar.readTVar currentTipV
        let tipChanged = case mcurrentUnconfirmedTip of
              Nothing -> True
              Just h  -> blockHeaderHeight h /= newUnconfirmedHeightTip
        when tipChanged $ do
           lift $! runLogging $ $(logInfo) $! "new unconfirmed tip is " <> tshow newUnconfirmedHeightTip
           tipHeader <- ExceptT $! liftIO $! fetchUnconfirmedTipHeader clientEnv config newUnconfirmedHeightTip
           liftIO $ STM.atomically $ TVar.writeTVar unconfirmedTipV (Just tipHeader)
           case mcurrentConfirmedTip of
             Just currentConfirmedTip
               | blockHeaderHeight currentConfirmedTip + configBlocksToConfirm config
                   >= newUnconfirmedHeightTip ->
                 liftIO $ STM.atomically $ TVar.writeTVar unconfirmedTipV (Just tipHeader)
             _ | newUnconfirmedHeightTip < configBlocksToConfirm config ->
                 liftIO $ STM.atomically $ TVar.writeTVar unconfirmedTipV (Just tipHeader)
             _ -> do
               let confirmedHeightFrom =
                     case mcurrentConfirmedTip of
                       Nothing -> 0
                       Just currentConfirmedTip -> blockHeaderHeight currentConfirmedTip + 1
                   confirmedHeightTo = newUnconfirmedHeightTip - configBlocksToConfirm config
               newestConfirmedBlockHeader <- ExceptT $! performSyncFromTo clientEnv
                 confirmedHeightFrom confirmedHeightTo
               lift $! runLogging $ $(logDebug) $ "new latest confirmed block height " <> tshow confirmedHeightTo
               liftIO $ STM.atomically $ do
                 TVar.writeTVar unconfirmedTipV (Just tipHeader)
                 TVar.writeTVar currentTipV (Just newestConfirmedBlockHeader)
  case eret of
    Right () -> return ()
    Left reason-> runLogging $! $(logError) reason
  where
    performSyncFromTo
      :: (MonadIO m, MonadMonitor m)
      => ClientEnv
      -> BlockHeight
      -> BlockHeight
      -> AppT m (Either Text BlockHeader)
    performSyncFromTo clientEnv confirmedHeightFrom confirmedHeightTo =
        let name = "performSyncFromTo"
        in profile name $ runExceptPrefixT name $ do
      lift $! Cache.ensureCapacity confirmedHeightTo
      mlastBH <- foldM  ( \_ height -> do -- fold over all blocks returning the last block header
          lift $! runLogging $ $(logDebug) $ "height " <> tshow height
          (bi, blockReward, chainReward) <- ExceptT $! getBlockInfos clientEnv height
          let bh = blockHeaderFromBlockInfos bi blockReward chainReward
          lift $! persistBlockHeader bh
          lift $! Cache.maybeInsert bh
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

        getBlockInfos
          :: (MonadIO m, MonadMonitor m)
          => ClientEnv
          -> BlockHeight
          -> AppT m (Either Text (BlockInfo, Word64, Word64))
        getBlockInfos clientEnv height =
            let name = "getBlockInfos"
            in profile name $ runExceptPrefixT name $ do
          State{ config = config
               , metrics = MetricsState { btcGetBlockHashH = btcGetBlockHashH
                                        , btcGetBlockH = btcGetBlockH
                                        , btcGetBlockStatsH = btcGetBlockStatsH
                                        }
               } <- lift ask
          let userPass = BasicAuthData (Text.encodeUtf8 $ configBTCUser config) (Text.encodeUtf8 $ configBTCPassword config)
          hash <- ExceptT $ fromResult $! liftIO $ P.observeDuration btcGetBlockHashH
              $ Bitcoin.withBitcoinEnv clientEnv $ getBlockHash userPass [height]
          bi <- ExceptT $ fromResult $! liftIO $ P.observeDuration btcGetBlockH
              $ Bitcoin.withBitcoinEnv clientEnv $ getBlock userPass [ hash ]
          blockReward <- do
            if height == 0
              then return 5000000000 {- default subsidy-}
              else do
                bs <- ExceptT $! fromResult $! liftIO $ P.observeDuration btcGetBlockStatsH
                    $ Bitcoin.withBitcoinEnv clientEnv $ getBlockStats userPass [height]
                return $! (BlockStats.totalfee bs + BlockStats.subsidy bs)
          chainReward <- do
            let isPreviousBlockChainRewardNeeded = height > 0
            if not isPreviousBlockChainRewardNeeded
              then return blockReward
              else do
                let prevBlockHeight = height - 1
                prevBlock <- exceptTMaybeT ("mgetBlockHeaderByHeight failed for height " <> tshow prevBlockHeight)
                  $ mgetBlockHeaderByHeight (height - 1)
                return (blockHeaderChainreward prevBlock + blockReward)
          return (bi, blockReward, chainReward)

        fromResult func = do
          rv <- func
          case rv of
            Right (Result _ v) -> return ( Right v)
            some -> return $! Left $! tshow some

        blockHeaderFromBlockInfos bi reward chainreward = BlockHeader
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
          , blockHeaderChainreward = chainreward
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

-- | fetches the full BlockHeader for the unconfirmed chain tip from
-- bitcoind. Guarded: returns Nothing on any RPC failure rather than
-- crashing the caller.
fetchUnconfirmedTipHeader :: ClientEnv-> Config -> BlockHeight -> IO (Either Text BlockHeader)
fetchUnconfirmedTipHeader clientEnv config tipHeight =
    let
      name = "fetchUnconfirmedTipHeader"
    in runExceptPrefixT name $ do
  let userPass = BasicAuthData
                   (Text.encodeUtf8 $ configBTCUser config)
                   (Text.encodeUtf8 $ configBTCPassword config)
  (bi, reward) <- ExceptT $ Bitcoin.withBitcoinEnv clientEnv $ do
      Result _ hash <- getBlockHash userPass [tipHeight]
      Result _ bi <- getBlock userPass [ hash ]
      if tipHeight == 0
        then return (bi, 5000000000)
        else do
          Result _ bs <- getBlockStats userPass [tipHeight]
          return (bi, BlockStats.totalfee bs + BlockStats.subsidy bs)
  return $! BlockHeader
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
    , blockHeaderChainreward = 0 -- not computed for unconfirmed tip
    }
