module OpEnergy.Server.V2.Core.SchedulerThread
  ( main
  , iteration
  ) where

import qualified Data.Text as Text
import           Data.Word(Word64)
import           Control.Monad.Reader(asks)
import           Control.Monad( when, foldM)
import           Control.Monad.Trans( lift)
import           Control.Monad.Trans.Except(ExceptT(..), withExceptT)
import qualified Control.Concurrent.STM.TVar as TVar
import           Data.Maybe( fromJust)
import           Flow

import           Data.OpEnergy.API.V1.Positive(fromPositive)
import           Data.OpEnergy.API.V1.Natural( verifyNatural)
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
      blockchainInfo <- ExceptT getBlockchainInfo
      configBlocksToConfirm <- asks <! Config.configBlocksToConfirm <. Env.config
      newUnconfirmedTipBlock <- ExceptT <! getBlockByHash
        <! Bitcoin.bestblockhash blockchainInfo
      catchBreakT <! do
        unconfirmedTipTheSame <- withExceptT Left <! ExceptT <! isUnconfirmedTipTheSame
          newUnconfirmedTipBlock
        when unconfirmedTipTheSame nothingShouldBeDone
        lift <! logDebug <! Text.unlines
          [ "new unconfirmed (height, hash) tip is: "
            <> tshow ( blockHeaderHeight newUnconfirmedTipBlock
                     , blockHeaderHash newUnconfirmedTipBlock
                     )
          , "configBlocksToConfirm: " <> tshow configBlocksToConfirm
          ]
        let
            isNewUnconfirmedTipBelowConfirmationTreshold =
              blockHeaderHeight newUnconfirmedTipBlock < configBlocksToConfirm
        when isNewUnconfirmedTipBelowConfirmationTreshold nothingShouldBeDone
        (eitherDiscoveredUnconfirmedTipReorganizedOrCurrentChainContinues
          :: Either (BlockHeader, BlockHeader) (BlockHeight, BlockHeight)
          ) <- withExceptT Left <! ExceptT
            <! checkEitherOldChainStalledOrContinues newUnconfirmedTipBlock
        case eitherDiscoveredUnconfirmedTipReorganizedOrCurrentChainContinues of
          Right (confirmedHeightFrom, confirmedHeightTo) -> do
            lift <! logInfo
              <! "observing new range of blocks within current branch: "
              <> tshow (confirmedHeightFrom, confirmedHeightTo)
            return <! Just (confirmedHeightFrom, confirmedHeightTo)
          Left (confirmedTipFromOldBranch, confirmedTipFromNewBranch) -> do
            lift <! logInfo <! Text.unlines
              [ "observing new branch, old confirmed block is now considered \
                \in a stale branch"
              , "old branch: " <> tshow confirmedTipFromOldBranch
              , "new branch: " <> tshow confirmedTipFromNewBranch
              ]
            rootBlockOfBothBranches <- withExceptT Left <! ExceptT
              <! searchCommonRootBlock
                confirmedTipFromOldBranch
                confirmedTipFromNewBranch
            let
                theFirstNewHeight = blockHeaderHeight rootBlockOfBothBranches + 1
            return <! Just
              ( theFirstNewHeight
              , blockHeaderHeight confirmedTipFromNewBranch
              )
      where
      nothingShouldBeDone = breakT Nothing

    checkEitherOldChainStalledOrContinues
      :: (Monad m)
      => BlockHeader
      -> (AppM transactionROM transactionM m)
         ( Either
           Failure
           ( Either
             (BlockHeader, BlockHeader)
             (BlockHeight, BlockHeight)
           )
         )
    checkEitherOldChainStalledOrContinues newUnconfirmedTipBlock =
        let name = "checkEitherOldChainStalledOrContinues"
        in runExceptPrefixTF name <! catchBreakT <! do
      configBlocksToConfirm <- asks <! Config.configBlocksToConfirm <. Env.config
      let
          confirmedBlockHeightOfNEWBranch = blockHeaderHeight newUnconfirmedTipBlock
            - configBlocksToConfirm
      mCurrentConfirmedTipV <- asks Env.mWitnessedUnconfirmedTipV
      mCurrentConfirmedTip <- lift <! atomically <! TVar.readTVar mCurrentConfirmedTipV
      currentConfirmedTip <- case mCurrentConfirmedTip of
        Nothing-> breakT <! Right
          ( verifyNatural 0
          , confirmedBlockHeightOfNEWBranch
          )
        Just some -> return some
      let
          newBranchConfirmedBlockHeightIsBelowCurrentConfirmedBlockHeight =
            blockHeaderHeight currentConfirmedTip > confirmedBlockHeightOfNEWBranch
      when newBranchConfirmedBlockHeightIsBelowCurrentConfirmedBlockHeight <! do
        confirmedTipOfTheNewBranch <- withExceptT Left <! ExceptT <! foldMDownChain
          (walkToHeight confirmedBlockHeightOfNEWBranch)
          newUnconfirmedTipBlock
        let
            oldBranchIsStalled = breakT <! Left
              ( currentConfirmedTip
              , confirmedTipOfTheNewBranch
              )
        oldBranchIsStalled
      return <! Right (blockHeaderHeight currentConfirmedTip, confirmedBlockHeightOfNEWBranch)

    searchCommonRootBlock
      :: Monad m
      => BlockHeader
      -> BlockHeader
      -> AppM transactonROM transactionM m (Either Failure BlockHeader)
    searchCommonRootBlock confirmedTipFromOldBranch confirmedTipFromNewBranch =
        let name = "searchCommonRootBlock"
        in runExceptPrefixTF name <! do
      let
          (shortestBranch, longestBranch) =
            if blockHeaderHeight confirmedTipFromOldBranch
               <= blockHeaderHeight confirmedTipFromNewBranch
              then (confirmedTipFromOldBranch, confirmedTipFromNewBranch)
              else (confirmedTipFromNewBranch, confirmedTipFromOldBranch)
      longestBranchsShortestHeight <- ExceptT <! foldMDownChain
        (walkToHeight (blockHeaderHeight shortestBranch)) longestBranch
      let
          bothBranchsAreTheSame =
            blockHeaderPreviousblockhash longestBranchsShortestHeight
            == blockHeaderPreviousblockhash shortestBranch
      case () of
        _ | bothBranchsAreTheSame -> return shortestBranch
        _ | otherwise -> do
          ExceptT <! foldMDownChain searchForCommonBlock
            (shortestBranch, longestBranchsShortestHeight)

    foldMDownChain
      :: (Monad m)
      => (acc -> AppM transactionROM tranasactionM m (Either Failure (Either acc result)))
      -> acc
      -> AppM transactionROM tranasactionM m (Either Failure result)
    foldMDownChain foo acc = loop acc
      where
      loop acc = do
        eContinueOrResult <- foo acc
        case eContinueOrResult of
          Left some -> return <! Left some
          Right (Left nextBlock) -> loop nextBlock
          Right (Right some) -> return <! Right some


    walkToHeight
      :: Monad m
      => BlockHeight
      -> BlockHeader
      -> AppM transactionROM transactionM m
        ( Either Failure (Either BlockHeader BlockHeader))
    walkToHeight targetHeight accBlock
      | isCurrentBlockIsTheTarget = return <! Right <! Right accBlock
      where
        isCurrentBlockIsTheTarget = blockHeaderHeight accBlock == targetHeight
    walkToHeight _targetHeight accBlock =
        let name = "walkToHeight"
        in runExceptPrefixTF name <! do
      previousBlockHash <- exceptTMaybeT ( Internal "no target height found")
        <! return <! blockHeaderPreviousblockhash accBlock
      previousBlock <- ExceptT
        <! getBlockByHash previousBlockHash
      return <! Left previousBlock

    searchForCommonBlock
      :: Monad m
      => (BlockHeader, BlockHeader)
      -> AppM transactionROM transactionM m
         (Either Failure (Either (BlockHeader, BlockHeader) BlockHeader))
    searchForCommonBlock (shortestBranchBlock, largestBranchBlock)
      | isCommonRootFound = return <! Right <! Right shortestBranchBlock
      where
        isCommonRootFound =
          blockHeaderHeight shortestBranchBlock == blockHeaderHeight largestBranchBlock
          && blockHeaderHash shortestBranchBlock == blockHeaderHash largestBranchBlock
    searchForCommonBlock (shortestBranchBlock, largestBranchBlock) =
        let name = "searchForCommonBlock"
        in runExceptPrefixTF name <! do
      shortestBranchPrevBlockHash <- exceptTMaybeT (Internal "shortest branch is empty")
        <! return <! blockHeaderPreviousblockhash shortestBranchBlock
      largestBranchPrevBlockHash <- exceptTMaybeT (Internal "longest branch is empty")
        <! return <! blockHeaderPreviousblockhash largestBranchBlock
      shortestBranchPrevBlock <- ExceptT <! getBlockByHash shortestBranchPrevBlockHash
      largestBranchPrevBlock <- ExceptT <! getBlockByHash largestBranchPrevBlockHash
      return <! Left (shortestBranchPrevBlock, largestBranchPrevBlock)


    isUnconfirmedTipTheSame newUnconfirmedTipBlock =
        let name = "isUnconfirmedTipTheSame"
        in runExceptPrefixTF name <! catchBreakT <! do
      mWitnessedUnconfirmedTipV <- asks Env.mWitnessedUnconfirmedTipV
      mWitnessedUnconfirmedTip <- lift <! atomically
        <! TVar.readTVar mWitnessedUnconfirmedTipV
      witnessedUnconfirmedTip <- case mWitnessedUnconfirmedTip of
        Nothing -> breakT False
        Just some -> return some
      return
        <! blockHeaderHeight witnessedUnconfirmedTip
           == blockHeaderHeight newUnconfirmedTipBlock
        && blockHeaderHash witnessedUnconfirmedTip
           == blockHeaderHash newUnconfirmedTipBlock

    updateLatestConfirmedHeightTip header =
        let name = "updateLatestConfirmedHeightTip"
        in profile name <! runExceptPrefixTF name <! do
      mCurrentConfirmedTipV <- lift <! asks Env.mCurrentConfirmedTipV
      lift <! atomically <! TVar.writeTVar mCurrentConfirmedTipV (Just header)

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
      mlastBH <- foldM  ( \_ height -> do -- fold over all blocks returning the last block header
          lift <! logDebug <! "height " <> tshow height
          (bi, blockReward, chainReward) <- ExceptT <! getBlockInfos height
          let bh = blockHeaderFromBlockInfos bi blockReward chainReward
          ExceptT <! persistBlockHeader bh
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

