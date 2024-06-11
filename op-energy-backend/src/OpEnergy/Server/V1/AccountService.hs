
module OpEnergy.Server.V1.AccountService where

import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import           Data.Pool(Pool)
import qualified Data.Map.Strict as Map
import           Servant.API (BasicAuthData(..))
import           Servant (err404, throwError)
import           Control.Monad (forM_)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text

import           Database.Persist.Postgresql

import           Data.OpEnergy.V1.Hash
import           Data.OpEnergy.V1.Account
import           OpEnergy.Server.V1.Config
import           OpEnergy.Server.V1.DB (tshow)
import           OpEnergy.Server.V1.Class (AppT, AppM, State(..))
import           Data.OpEnergy.API.V1.Error(throwJSON)


register :: AppM RegisterResult
register = do


getBlockHeaderByHash :: BlockHash -> AppM BlockHeader
getBlockHeaderByHash hash = do
  State{ blockHeadersDBPool = pool, blockHeadersHashCache = blockHeadersHashCacheV, blockHeadersHeightCache = blockHeadersHeightCacheV } <- ask
  blockHeadersHashCache <- liftIO $ TVar.readTVarIO blockHeadersHashCacheV
  case Map.lookup hash blockHeadersHashCache of -- check cache first
    Just height -> do
      liftIO $ Text.putStrLn $ "hash " <> tshow hash <> " is a height " <> tshow height <> " and had been found in the cache"
      getBlockHeaderByHeight height
    Nothing -> do -- there is no header in cache
      mheader <- liftIO $ flip runSqlPersistMPool pool $ selectFirst [ BlockHeaderHash ==. hash ] []
      case mheader of
        Nothing-> throwJSON err400 ("block with given hash was not found"::Text)
        Just (Entity _ header) -> do
          height <- liftIO $ STM.atomically $ do
            let height = blockHeaderHeight header
            TVar.modifyTVar blockHeadersHeightCacheV $ \cache-> Map.insert height header cache -- update cache
            TVar.modifyTVar blockHeadersHashCacheV $ \cache-> Map.insert hash height cache -- update cache
            return height
          liftIO $ Text.putStrLn $ "header with height " <> tshow height <> " and hash " <> tshow hash <> " inserted into the cache"
          return header

getBlockHeaderByHeight :: BlockHeight -> AppM BlockHeader
getBlockHeaderByHeight height = do
  State{ blockHeadersDBPool = pool, currentHeightTip = currentHeightTipV, blockHeadersHashCache = blockHeadersHashCacheV, blockHeadersHeightCache = blockHeadersHeightCacheV } <- ask
  mcurrentHeightTip <- liftIO $ TVar.readTVarIO currentHeightTipV
  blockHeadersHeightCache <- liftIO $ TVar.readTVarIO blockHeadersHeightCacheV
  case mcurrentHeightTip of
    Just currentHeightTip
      | height < currentHeightTip -> do
          case Map.lookup height blockHeadersHeightCache of -- check cache first
            Just header -> do
              liftIO $ Text.putStrLn $ "header with height " <> tshow height <> " found in the cache"
              return header
            Nothing -> do -- there is no header in cache
              mheader <- liftIO $ flip runSqlPersistMPool pool $ selectFirst [ BlockHeaderHeight ==. height ] []
              case mheader of
                Nothing-> throwJSON err400 ("no block found with given height"::Text)
                Just (Entity _ header) -> do
                  liftIO $ STM.atomically $ do
                    TVar.modifyTVar blockHeadersHeightCacheV $ \cache-> Map.insert height header cache -- update cache
                    TVar.modifyTVar blockHeadersHashCacheV $ \cache-> Map.insert (blockHeaderHash header) height cache -- update cache
                  liftIO $ Text.putStrLn $ "header with height " <> tshow height <> " inserted in the cache"
                  return header
    _ -> throwJSON err400 ("given height is in the future"::Text)

mgetLastBlockHeader :: Pool SqlBackend-> IO (Maybe (Entity BlockHeader))
mgetLastBlockHeader pool = flip runSqlPersistMPool pool $ selectFirst ([] :: [Filter BlockHeader]) [ Desc BlockHeaderHeight ]

-- | performs read from DB in order to set State.currentHeightTip
loadDBState :: AppT IO ()
loadDBState = do
  State{ blockHeadersDBPool = pool, currentHeightTip = currentHeightTipV } <- ask
  mlast <- liftIO $ mgetLastBlockHeader pool
  case mlast of
    Nothing-> return () -- do nothing
    Just (Entity _ header) -> liftIO $ do
      STM.atomically $ TVar.writeTVar currentHeightTipV (Just (blockHeaderHeight header))
      Text.putStrLn ("current confirmed height tip " <> tshow (blockHeaderHeight header))

-- | this procedure ensures that BlockHeaders table is in sync with block chain
syncBlockHeaders :: AppT IO ()
syncBlockHeaders = do
  liftIO $ Text.putStrLn "syncBlockHeaders"
  mstartSyncHeightFromTo <- mgetHeightToStartSyncFromTo
  case mstartSyncHeightFromTo of
    Nothing-> return () -- do nothing if sync is not needed
    Just (startSyncHeightFrom, startSyncHeightTo) -> do
      performSyncFromTo startSyncHeightFrom startSyncHeightTo
      liftIO $ Text.putStrLn $ "new latest confirmed block height " <> tshow startSyncHeightTo
      updateLatestConfirmedHeightTip startSyncHeightTo
  where
    updateLatestConfirmedHeightTip startSyncHeightTo = do
      State{ currentHeightTip = currentHeightTipV } <- ask
      liftIO $ STM.atomically $ TVar.writeTVar currentHeightTipV (Just startSyncHeightTo)

    mgetHeightToStartSyncFromTo = do
      State{ config = config, currentHeightTip = currentHeightTipV } <- ask
      mcurrentConfirmedHeightTip <- liftIO $ TVar.readTVarIO currentHeightTipV
      let userPass = BasicAuthData (Text.encodeUtf8 $ configBTCUser config) (Text.encodeUtf8 $ configBTCPassword config)
      eblockchainInfo <- liftIO $ Bitcoin.withBitcoin ( configBTCURL config) (getBlockchainInfo userPass [])
      case eblockchainInfo of
        (Result _ blockchainInfo ) -> do
          let newUnconfirmedHeightTip = Bitcoin.blocks blockchainInfo
          liftIO $ Text.putStrLn ( "current unconfirmed height tip is " <> tshow newUnconfirmedHeightTip)
          case mcurrentConfirmedHeightTip of
            Just currentConfirmedHeightTip
              | currentConfirmedHeightTip + (configBlocksToConfirm config) == newUnconfirmedHeightTip -> return Nothing
            _ | newUnconfirmedHeightTip < (configBlocksToConfirm config) -> return Nothing -- do nothing, if there are no confirmed blocks yet
            _ -> do -- there are some confirmed blocks, that we are not aware of, need to sync DB
              let confirmedHeightFrom =
                    case mcurrentConfirmedHeightTip of
                      Nothing -> 0 -- no previously confirmed tip, start with 0
                      Just some -> (some + 1) -- start with the next unconfired tip
                  confirmedHeightTo = newUnconfirmedHeightTip - (configBlocksToConfirm config)
              return $ Just (confirmedHeightFrom, confirmedHeightTo)
        some -> error ( "syncBlockHeaders: getBlockchainInfo error: " ++ show some)

    performSyncFromTo confirmedHeightFrom confirmedHeightTo = do
      forM_ [ confirmedHeightFrom .. confirmedHeightTo ] $ \height -> do
        liftIO $ Text.putStrLn $ "height " <> tshow height
        (bi, reward) <- getBlockInfos height
        let bh = blockHeaderFromBlockInfos bi reward
        persistBlockHeader bh
      where
        persistBlockHeader :: BlockHeader -> AppT IO ()
        persistBlockHeader header = do
          State{ blockHeadersDBPool = pool } <- ask
          _ <- liftIO $ flip runSqlPersistMPool pool $ insert header
          return ()

        getBlockInfos height = do
          State{ config = config } <- ask
          let userPass = BasicAuthData (Text.encodeUtf8 $ configBTCUser config) (Text.encodeUtf8 $ configBTCPassword config)
          liftIO $ Bitcoin.withBitcoin ( configBTCURL config) $ do
            Result _ hash <- getBlockHash userPass [height]
            liftIO $ Text.putStrLn $ "getBlockHash " <> tshow hash
            Result _ bi <- getBlock userPass [ hash ]
            liftIO $ Text.putStrLn $ "getBlock " <> tshow bi
            if height == 0
              then return (bi, 5000000000 {- default subsidy-} )
              else do
                Result _ bs <- getBlockStats userPass [height]
                liftIO $ Text.putStrLn $ "getBlockStats " <> tshow bs
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
