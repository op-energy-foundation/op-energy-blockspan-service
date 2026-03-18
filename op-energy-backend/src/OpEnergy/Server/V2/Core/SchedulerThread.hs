module OpEnergy.Server.V2.Core.SchedulerThread
  ( main
  , iteration
  ) where

import           Data.Word(Word64)
import           Control.Monad.Reader(asks)
import           Control.Monad( when, foldM)
import           Control.Monad.Trans( lift)
import           Control.Monad.Trans.Except(ExceptT(..))
import qualified Control.Concurrent.STM.TVar as TVar
import           Data.Maybe( fromJust)
import           Flow

import           Data.OpEnergy.API.V1.Positive(fromPositive)
import           Data.OpEnergy.API.V1.Block(BlockHeight, BlockHeader(..))
import qualified Data.Bitcoin.BlockInfo as BlockInfo
import qualified Data.Bitcoin.BlockStats as BlockStats
import qualified Data.Bitcoin.API as Bitcoin
import           Data.Text.Show( tshow)

import           OpEnergy.Server.Common
import qualified OpEnergy.Server.V1.Config as Config
import           OpEnergy.Server.V2.Core.Call
import           OpEnergy.Server.V2.Core.HandleRequest.GetBlockHeaderByHeight
import           OpEnergy.Server.V2.Core.App(AppM)
import           OpEnergy.Server.V2.Environment.Profiler
import           OpEnergy.Server.V2.Environment.Logger
import           OpEnergy.Server.V2.Environment.IOM
import           OpEnergy.Server.V2.Environment.STMM
import           OpEnergy.Server.V2.Environment.BitcoinClient
import           OpEnergy.Server.V2.Environment.DataSource
import qualified OpEnergy.Server.V2.Environment.DataSource.Class as DataSource
import qualified OpEnergy.Server.V2.Environment as Env

main
  :: ( Monad m
     , Monad transactionM
     , Monad transactionROM
     )
  => AppM transactionROM transactionM m ()
main = loop
  where
  loop = do
    delay <- asks ( Env.config
                  .> Config.configSchedulerPollRateSecs
                  .> fromPositive
                  )
    shouldContinue <- iteration
    threadDelay delay
    if shouldContinue
      then loop
      else logInfo "scheduler thread shutdown"

iteration
  :: ( Monad m
     , Monad transactionM
     , Monad transactionROM
     )
  => AppM transactionROM transactionM m Bool
iteration =
    let name = "SchedulerThread.iteration"
    in profile name <! do
  shutdownRequestedV <- asks Env.shutdownRequestedV
  shutdownRequested <- atomically <! TVar.readTVar shutdownRequestedV
  if shutdownRequested
    then return False
    else do
      eret <- syncBlockHeaders
      case eret of
        Left reason -> do
          logError <! tshow reason
          return False
        Right () -> return True

syncBlockHeaders
  :: ( Monad m
     , Monad transactionM
     , Monad transactionROM
     )
  => AppM transactionROM transactionM m (Either Failure ())
syncBlockHeaders =
    let name = "syncBlockHeaders"
    in profile name <! runExceptPrefixTF name <! do
  mstartSyncHeightFromTo <- ExceptT <! mgetHeightToStartSyncFromTo
  case mstartSyncHeightFromTo of
    Nothing-> return () -- do nothing if sync is not needed
    Just (startSyncHeightFrom, startSyncHeightTo) -> do
      lift <! logInfo <! "new latest confirmed block height to sync "
        <> tshow startSyncHeightTo
      newestConfirmedBlockHeader <- ExceptT <! performSyncFromTo
        startSyncHeightFrom
        startSyncHeightTo
      ExceptT <! updateLatestConfirmedHeightTip newestConfirmedBlockHeader
        -- cache newest header
  return ()
  where
    -- | queries bitcoin node and compares with latest witnessed block
    mgetHeightToStartSyncFromTo
      :: Monad m
      => AppM transactionROM transactionM m
        (Either Failure (Maybe (BlockHeight, BlockHeight)))
    mgetHeightToStartSyncFromTo =
        let name = "mgetHeightToStartSyncFromTo"
        in profile name <! runExceptPrefixTF name <! do
      config <- asks Env.config
      mcurrentConfirmedTipV <- asks Env.mcurrentTipV
      mcurrentConfirmedTip <- lift <! atomically
        <! TVar.readTVar mcurrentConfirmedTipV
      blockchainInfo <- ExceptT <! getBlockchainInfo
      let newUnconfirmedHeightTip = Bitcoin.blocks blockchainInfo
      lift <! logDebug <! "current unconfirmed height tip is: "
        <> tshow newUnconfirmedHeightTip
      catchBreakT <! do
        case mcurrentConfirmedTip of
          Just currentConfirmedTip
            | blockHeaderHeight currentConfirmedTip
              + Config.configBlocksToConfirm config
              >= newUnconfirmedHeightTip ->
              let newConfirmedBlockNotFoundYet = breakT Nothing
              in newConfirmedBlockNotFoundYet
          _ ->
            let continue = return ()
            in continue
        when ( newUnconfirmedHeightTip < Config.configBlocksToConfirm config) <!
          let thereIsNoConfirmedBlockFoundYet = breakT Nothing
          in thereIsNoConfirmedBlockFoundYet
        let confirmedHeightFrom =
              case mcurrentConfirmedTip of
                Nothing -> 0 -- no previously confirmed tip, start with 0
                Just currentConfirmedTip ->
                  blockHeaderHeight currentConfirmedTip + 1 -- start with the
                    -- next unconfirmed tip
            confirmedHeightTo = newUnconfirmedHeightTip
              - Config.configBlocksToConfirm config
        return <! Just (confirmedHeightFrom, confirmedHeightTo)


    updateLatestConfirmedHeightTip header =
        let name = "updateLatestConfirmedHeightTip"
        in profile name <! runExceptPrefixTF name <! do
      mcurrentTipV <- lift <! asks Env.mcurrentTipV
      lift <! atomically <! TVar.writeTVar mcurrentTipV (Just header)



    performSyncFromTo
      :: ( Monad m
         , Monad transactionM
         , Monad transactionROM
         )
      => BlockHeight
      -> BlockHeight
      -> AppM transactionROM transactionM m (Either Failure BlockHeader)
    performSyncFromTo confirmedHeightFrom confirmedHeightTo =
        let name = "performSyncFromTo"
        in profile name <! runExceptPrefixTF name <! do
      -- Cache.ensureCapacity confirmedHeightTo
      mlastBH <- foldM  ( \_ height -> do -- fold over all blocks returning the last block header
          lift <! logDebug <! "height " <> tshow height
          (bi, blockReward, chainReward) <- ExceptT <! getBlockInfos height
          let bh = blockHeaderFromBlockInfos bi blockReward chainReward
          ExceptT <! persistBlockHeader bh
          -- Cache.maybeInsert bh
          return <! Just bh
        )
        Nothing
        [ confirmedHeightFrom .. confirmedHeightTo ]
      return <! fromJust mlastBH
      where
        persistBlockHeader
          :: ( Monad m
             , Monad transactionM
             )
          => BlockHeader
          -> AppM transactionROM transactionM m (Either Failure ())
        persistBlockHeader header =
            let name = "persistBlockHeader"
            in profile name <! runExceptPrefixTF name <! do
          dataSource <- lift <! asks Env.dataSource
          ExceptT <! withTransaction
            <! DataSource.storeBlockHeader dataSource header
          return ()

        getBlockInfos
          :: ( Monad m
             , Monad transactionROM
             )
          => BlockHeight
          -> AppM transactionROM transactionM m
            (Either Failure (BlockInfo.BlockInfo, Word64, Word64))
        getBlockInfos height =
            let name = "getBlockInfos"
            in profile name <! runExceptPrefixTF name <! do
          hash <- ExceptT <! getBlockHash height
          bi <- ExceptT <! getBlock hash
          blockReward <-
            if height == 0
              then return 5000000000 {- default subsidy-}
              else do
                bs <- ExceptT <! getBlockStats height
                return <! (BlockStats.totalfee bs + BlockStats.subsidy bs)
          chainReward <- do
            let isPreviousBlockChainRewardNeeded = height > 0
            if not isPreviousBlockChainRewardNeeded
              then return blockReward
              else do
                prevBlock <- ExceptT <! getBlockHeaderByHeight (height - 1)
                return <! blockHeaderChainreward prevBlock + blockReward
          return (bi, blockReward, chainReward)

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

