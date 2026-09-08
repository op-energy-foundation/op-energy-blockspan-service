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
import           Servant.Client.JsonRpc
import           Control.Monad (foldM, when)
import           Control.Monad.Logger (logDebug, logInfo, logError, logWarn)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.Trans.Except (runExceptT, ExceptT(..))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Exception.Safe as E
import           Data.Text( Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Text.Show (tshow)
import           Data.Word

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
       , unconfirmedTip = unconfirmedTipV
       , config = config
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
        -- fetch the unconfirmed tip so it is available from the first
        -- websocket message, not only after the next block
        let userPass = BasicAuthData (Text.encodeUtf8 $ configBTCUser config) (Text.encodeUtf8 $ configBTCPassword config)
        liftIO $ E.handle (\(_ :: E.SomeException) ->
          return ()
          ) $ do
            ebi <- Bitcoin.withBitcoin (configBTCURL config) (getBlockchainInfo userPass [])
            case ebi of
              (Result _ blockchainInfo) -> do
                let tipHeight = Bitcoin.blocks blockchainInfo
                mTipHeader <- fetchUnconfirmedTipHeader config tipHeight
                case mTipHeader of
                  Just tipHeader -> STM.atomically $ TVar.writeTVar unconfirmedTipV (Just tipHeader)
                  Nothing -> return ()
              _ -> return ()

-- | this procedure ensures that BlockHeaders table is in sync with block chain,
-- and keeps the unconfirmed tip header up to date for the websocket.
syncBlockHeaders :: (MonadIO m, MonadMonitor m) => AppT m ()
syncBlockHeaders = do
  State{ config = config
       , currentTip = currentTipV
       , unconfirmedTip = unconfirmedTipV
       , metrics = MetricsState{ syncBlockHeadersH = syncBlockHeadersH
                                , btcGetBlockchainInfoH = btcGetBlockchainInfoH
                                }
       } <- ask
  runLogging $ $(logDebug) "syncBlockHeaders"
  P.observeDuration syncBlockHeadersH $ do
    -- query bitcoind for the current chain tip
    let userPass = BasicAuthData (Text.encodeUtf8 $ configBTCUser config) (Text.encodeUtf8 $ configBTCPassword config)
    eblockchainInfo <- liftIO $ P.observeDuration btcGetBlockchainInfoH $ Bitcoin.withBitcoin (configBTCURL config) (getBlockchainInfo userPass [])
    case eblockchainInfo of
      (Result _ blockchainInfo) -> do
        let newUnconfirmedHeightTip = Bitcoin.blocks blockchainInfo
        runLogging $ $(logDebug) ("current unconfirmed height tip is " <> tshow newUnconfirmedHeightTip)

        -- update unconfirmed tip header when height changes
        mcurrentUnconfirmedTip <- liftIO $ TVar.readTVarIO unconfirmedTipV
        let tipChanged = case mcurrentUnconfirmedTip of
              Nothing -> True
              Just h  -> blockHeaderHeight h /= newUnconfirmedHeightTip
        when tipChanged $ do
          mTipHeader <- liftIO $ fetchUnconfirmedTipHeader config newUnconfirmedHeightTip
          case mTipHeader of
            Just tipHeader -> liftIO $ STM.atomically $ TVar.writeTVar unconfirmedTipV (Just tipHeader)
            Nothing -> runLogging $ $(logWarn) "failed to fetch unconfirmed tip header"

        -- sync confirmed blocks if needed
        mcurrentConfirmedTip <- liftIO $ TVar.readTVarIO currentTipV
        case mcurrentConfirmedTip of
          Just currentConfirmedTip
            | blockHeaderHeight currentConfirmedTip + (configBlocksToConfirm config) >= newUnconfirmedHeightTip -> return ()
          _ | newUnconfirmedHeightTip < (configBlocksToConfirm config) -> return ()
          _ -> do
            let confirmedHeightFrom =
                  case mcurrentConfirmedTip of
                    Nothing -> 0
                    Just currentConfirmedTip -> (blockHeaderHeight currentConfirmedTip + 1)
                confirmedHeightTo = newUnconfirmedHeightTip - (configBlocksToConfirm config)
            newestConfirmedBlockHeader <- performSyncFromTo confirmedHeightFrom confirmedHeightTo
            runLogging $ $(logDebug) $ "new latest confirmed block height " <> tshow confirmedHeightTo
            updateLatestConfirmedHeightTip newestConfirmedBlockHeader
      some -> error ("syncBlockHeaders: getBlockchainInfo error: " ++ show some)
  where
    updateLatestConfirmedHeightTip header = do
      State{ currentTip = currentTipV } <- ask
      liftIO $ STM.atomically $ TVar.writeTVar currentTipV (Just header)

    performSyncFromTo confirmedHeightFrom confirmedHeightTo = do
      Cache.ensureCapacity confirmedHeightTo
      mlastBH <- foldM  ( \_ height -> do -- fold over all blocks returning the last block header
          runLogging $ $(logDebug) $ "height " <> tshow height
          (bi, blockReward, chainReward) <- getBlockInfos height
          let bh = blockHeaderFromBlockInfos bi blockReward chainReward
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

        getBlockInfos
          :: (MonadIO m, MonadMonitor m)
          => BlockHeight
          -> AppT m (BlockInfo, Word64, Word64)
        getBlockInfos height = do
          State{ config = config
               , metrics = MetricsState { btcGetBlockHashH = btcGetBlockHashH
                                        , btcGetBlockH = btcGetBlockH
                                        , btcGetBlockStatsH = btcGetBlockStatsH
                                        }
               } <- ask
          let userPass = BasicAuthData (Text.encodeUtf8 $ configBTCUser config) (Text.encodeUtf8 $ configBTCPassword config)
          eret <- runExceptT $ do
            hash <- ExceptT $ do
              response <- liftIO $ P.observeDuration btcGetBlockHashH
                $ Bitcoin.withBitcoin ( configBTCURL config) $ getBlockHash userPass [height]
              case response of
                Result _ hash -> return $! Right hash
                _ -> return $! Left $! Text.pack $! show response
            bi <- ExceptT $ do
              response <- liftIO $ P.observeDuration btcGetBlockH
                $ Bitcoin.withBitcoin ( configBTCURL config) $ getBlock userPass [ hash ]
              case response of
                Result _ bi -> return $! Right bi
                _ -> return $! Left $! "getBlock returned " <> Text.pack (show response)
            blockReward <- ExceptT $ do
              if height == 0
                then return $! Right 5000000000 {- default subsidy-}
                else do
                  response <- liftIO $ P.observeDuration btcGetBlockStatsH
                    $ Bitcoin.withBitcoin ( configBTCURL config) $ getBlockStats userPass [height]
                  case response of
                    Result _ bs -> return $! Right $! (BlockStats.totalfee bs + BlockStats.subsidy bs)
                    _ -> return $! Left $! "getBlockStats returned: "
                                        <> Text.pack (show response)
            chainReward <- ExceptT $ do
              let isPreviousBlockChainRewardNeeded = height > 0
              if not isPreviousBlockChainRewardNeeded
                then return $! Right blockReward
                else do
                  mprevBlock <- mgetBlockHeaderByHeight (height - 1)
                  case mprevBlock of
                    Just prevBlock -> return $! Right (blockHeaderChainreward prevBlock + blockReward)
                    Nothing -> return $! Left $! "mgetBlockHeaderByHeight failed for height "
                                              <> Text.pack (show (height - 1))
            return (bi, blockReward, chainReward)
          case eret of
            Right ret -> return ret
            Left reason -> do
              let
                  err = "getBlockInfos: " <> reason <> ", crashing to retry"
              runLogging $ $(logError) err
              error (Text.unpack err)

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
fetchUnconfirmedTipHeader :: Config -> BlockHeight -> IO (Maybe BlockHeader)
fetchUnconfirmedTipHeader config tipHeight = do
  let userPass = BasicAuthData (Text.encodeUtf8 $ configBTCUser config) (Text.encodeUtf8 $ configBTCPassword config)
  E.handle (\(_ :: E.SomeException) -> return Nothing) $ do
    (bi, reward) <- Bitcoin.withBitcoin (configBTCURL config) $ do
      Result _ hash <- getBlockHash userPass [tipHeight]
      Result _ bi <- getBlock userPass [ hash ]
      if tipHeight == 0
        then return (bi, 5000000000)
        else do
          Result _ bs <- getBlockStats userPass [tipHeight]
          return (bi, BlockStats.totalfee bs + BlockStats.subsidy bs)
    return $! Just $! BlockHeader
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
