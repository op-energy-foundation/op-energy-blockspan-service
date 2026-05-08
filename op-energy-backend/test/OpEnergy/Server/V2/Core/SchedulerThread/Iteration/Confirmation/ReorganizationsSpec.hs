module OpEnergy.Server.V2.Core.SchedulerThread.Iteration.Confirmation.ReorganizationsSpec
  ( spec
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Control.Monad( unless, foldM, forM_, void)
import           Control.Monad.Trans( lift)
import           Control.Monad.Trans.Except( runExceptT, ExceptT(..))
import           Control.Concurrent.STM(STM)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Data.Map.Strict as Map
import           Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import           Flow

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import qualified Test.QuickCheck as QC
-- import qualified Prometheus as P

import           Data.Text.Show
import           Data.OpEnergy.API.V1.Natural
                   ( Natural
                   , verifyNatural
                   , fromNatural
                   )
import qualified Data.OpEnergy.API.V1.Block as Block

import           OpEnergy.Server.V2.Core.Call(Failure(..))
import           OpEnergy.Server.V2.Core.App( runAppM)
import qualified OpEnergy.Server.V2.Core.SchedulerThread as Scheduler
import qualified OpEnergy.Server.V2.Core.HandleRequest.GetBlockHeaderByHeight
                   as GetBlockHeaderByHeight
import qualified OpEnergy.Server.V2.Core.HandleRequest.GetBlockHeaderByHash
                   as GetBlockHeaderByHash
import qualified OpEnergy.Server.V2.Environment as Env
import qualified OpEnergy.Server.V2.Environment.Test as Env
import qualified OpEnergy.Server.V2.Environment.BitcoinClient.Test as Bitcoin
import qualified OpEnergy.Server.V2.Environment.BitcoinClient as Bitcoin
import qualified OpEnergy.Server.V2.Environment.Time.Manual as Time
import qualified OpEnergy.Server.V2.Environment.DataSource.Test as DataSource
import qualified OpEnergy.Server.V2.Environment.Profiler.Dummy as Profiler
import qualified OpEnergy.Server.V1.Config as Config

spec :: Spec
spec = do
  (profilerProdState, _) <- runIO <! Profiler.init Nothing

  describe "scheduler.iteration.confirmations.reorgs.from_top" <! do
    prop_witnessingBlockChainFromTheTopShouldNotWitnessStalledBranches
      profilerProdState

  describe "scheduler.iteration.confirmations.reorgs.from_bottom" <! do
    prop_witnessingBlockChainFromTheBottomShouldNotWitnessStalledBranches
      profilerProdState

  describe "scheduler.iteration.confirmations.reorgs.from_bottom.unexpected_reorgs.by1" <! do
    prop_witnessingBlockChainFromTheBottomShouldHandleStalledBrancheOverConfigBarrierBy1
      profilerProdState

  describe "scheduler.iteration.confirmations.reorgs.from_bottom.unexpected_reorgs.byMoreThan1" <! do
    prop_witnessingBlockChainFromTheBottomShouldWitnessStalledBranchesAndNotShutdown
      profilerProdState

  fdescribe "scheduler.iteration.confirmations.reorgs.from_bottom.unexpected_reorgs.should_keep_stale_and_new_branch" <! do
    prop_witnessingBlockChainFromTheBottomShouldWitnessStalledBranchesAndKeepBoth
      profilerProdState


    -- when we use production profiler, we can output stats for benchmarks and etc
    -- it "should log and clear" <! do
    --   Text.putStrLn =<< Profiler.metricsSerialize
    --   P.unregisterAll


diagInfo
  :: Text
  -> Natural Int
  -> Block.BlockHeader
  -> Vector Block.BlockHeader
  -> Block.BlockHeader
  -> Env.State
  -> Env.Environment STM STM IO
  -> PropertyM IO Text
diagInfo
    description
    configBlocksToConfirm
    tip
    blockchain
    block
    envState
    env = run <! do
  mcurrentConfirmedTip <- TVar.readTVarIO <! Env.mCurrentConfirmedTipV env
  mWitnessedUnconfirmedTip <- TVar.readTVarIO <! Env.mWitnessedUnconfirmedTipV env
  blocks <- TVar.readTVarIO <! DataSource.blocksV dataSourceState
  requestLog <- TVar.readTVarIO <! Env.requestLogV envState
  (mtipBlock, _) <- TVar.readTVarIO <! Bitcoin.mtipBlockV
    <! Env.bitcoinClientState envState
  let
      lastRequests = Seq.viewr requestLog
        !> F.foldl'
          (\acc item-> tshow item:acc)
          []
        !> List.take 40
        !> List.reverse
      mchainTipBlock = case mtipBlock of
        Nothing -> Nothing
        Just chainTip -> blockchain !? chainTip
  return <! Text.unlines
    <!
      [ description
      , "size of persisted blockchain = " <> tshow ( Map.size blocks)
      , "blockHeight of the last processed block = " <> tshow blockHeight
      , "configBlocksToConfirm = " <> tshow configBlocksToConfirm
      , "bitcoin client tip = " <> tshow ( Block.blockHeaderHeight tip
                                         , Block.blockHeaderHash tip
                                         , Block.blockHeaderPreviousblockhash tip
                                         )
      , "mcurrentConfirmedTip = " <> tshow mcurrentConfirmedTip
      , "mWitnessedUnconfirmedTip = " <> tshow mWitnessedUnconfirmedTip
      , "mchainTipBlock = " <> tshow mchainTipBlock
      , "block = " <> tshow block
      ]
      ++ lastRequests
  where
    dataSourceState = Env.dataSourceState envState
    blockHeight = Block.blockHeaderHeight block


prop_witnessingBlockChainFromTheTopShouldNotWitnessStalledBranches
  :: Profiler.State
  -> SpecWith ()
prop_witnessingBlockChainFromTheTopShouldNotWitnessStalledBranches
    profilerProdState =
    let name = "prop_should_workaround_reorganizations_within_config_limit"
    in it (Text.unpack name)
      <! QC.forAll
         ( generatePropConfig (\_ same -> return same))
      <! \( configBlocksToConfirmInt :: Int
          , tipRawInt :: Int
          , reorgzOffset :: [Int]
          ) -> monadicIO <! do
  let
      configBlocksToConfirm = verifyNatural configBlocksToConfirmInt
      tipInt = tipRawInt + configBlocksToConfirmInt
      tip = verifyNatural tipInt
  blockchain <- run <! Bitcoin.generateBlockChain Bitcoin.genesisMediantime 0
    tip
  eblockchainWithReorgs <- run <! Bitcoin.addReorganizations reorgzOffset blockchain
  (blockchainWithReorgs, reorganaizedBlocks) <- case eblockchainWithReorgs of
    Right some -> return some
    Left some -> fail <! Text.unpack <! name <> ": " <> tshow some
  let
      blockchainWithReorgsSz = V.length blockchainWithReorgs
  (envState, env) <- lift <! Env.initConfigProfilerProdState
    name
    (\c -> c { Config.configBlocksToConfirm = configBlocksToConfirm })
    (Just profilerProdState)
    blockchainWithReorgs
  let
      lastblock = blockchainWithReorgs ! (blockchainWithReorgsSz - 1)
  run <! Time.setTimeNS (Env.timeState envState)
    (fromIntegral (Block.blockHeaderTimestamp lastblock) * 1_000_000_000)
  shouldContinue <- run <! runAppM name env <! do
    Scheduler.iteration
  unless shouldContinue
    <! (fail <. Text.unpack)
    =<< diagInfo
      (name <> ": iteration's fail unexpected")
      configBlocksToConfirm
      lastblock
      blockchainWithReorgs
      lastblock
      envState
      env

  prop_sizeOfConfirmedBlockchainIsTheSameAsInitialBlockChain envState env
    blockchain lastblock

  prop_confirmedBlockchainShouldBeTheSameAsInitialChainButWithNewBranches
    envState env blockchain reorganaizedBlocks

-- (1,36,[1,1,1,1,6])
generatePropConfig
  :: (Int -> [Int] -> Gen [Int])
  -> Gen (Int, Int, [Int])
generatePropConfig adjustReorgz = do
  -- blocksToConfirm <- return 1
  blocksToConfirm <- QC.choose (1, 20)
  (initialReorgzCount :: Int) <- QC.choose (2, 5)
  initialReorgz :: [Int] <- foldM
    ( \acc _ -> do
      v <- QC.choose (1, blocksToConfirm)
      return <! v:acc
    )
    []
    [ 1 .. initialReorgzCount ]
  -- adjustedReorgz <- return [1,1,1,1,6]
  adjustedReorgz <- adjustReorgz blocksToConfirm initialReorgz
  let
      maxReorg = List.foldl' max (head adjustedReorgz) (tail adjustedReorgz)
      reorgzCount = List.length adjustedReorgz
      blocksCountRequiredToConnectNewBranch = 2 -- first + last blocks
      minimumBlockchainChunkSize = maxReorg + blocksCountRequiredToConnectNewBranch
  tipRawInt <- QC.choose ( minimumBlockchainChunkSize * reorgzCount
                         , minimumBlockchainChunkSize * (reorgzCount + 1)
                         )
  return (blocksToConfirm, tipRawInt, adjustedReorgz)

prop_sizeOfConfirmedBlockchainIsTheSameAsInitialBlockChain
  :: Env.State
  -> Env.Environment STM STM IO
  -> Vector Block.BlockHeader
  -> Block.BlockHeader
  -> PropertyM IO ()
prop_sizeOfConfirmedBlockchainIsTheSameAsInitialBlockChain envState env
    blockchain lastblock =
    let name = "prop_sizeOfConfirmedBlockchainIsTheSameAsInitialBlockChain"
    in do
  confirmedBlocks <- run <! TVar.readTVarIO <! DataSource.blocksV dataSourceState
  mcurrentConfirmedTip <- run <! TVar.readTVarIO <! Env.mCurrentConfirmedTipV env
  currentTimeNS <- run <! TVar.readTVarIO <! Time.nanosecondsV (Env.timeState envState)
  let
      currentTime = currentTimeNS `div` 1_000_000_000
      confirmedBlockchainSize = Map.size confirmedBlocks
      sizeOfConfirmedBlockchainIsTheSameAsInitialBlockChain =
        confirmedBlockchainSize == sizeOfInitialBlockchain - configBlocksToConfirmInt
  unless sizeOfConfirmedBlockchainIsTheSameAsInitialBlockChain <! do
    requestLog <- run <! TVar.readTVarIO <! Env.requestLogV envState
    let
        plainBlockchain = Bitcoin.blockChain bitcoinState
        plainBlockchainSz = V.length plainBlockchain
        lastRequests = Seq.viewr requestLog
          !> F.foldl'
            (\acc item-> tshow item:acc)
            []
          !> List.take 2000
          !> List.reverse
    (mtipIdx, _) <- run <! TVar.readTVarIO <! Bitcoin.mtipBlockV bitcoinState
    let
        munconfirmedTip = do
          tipIndex <- mtipIdx
          plainBlockchain !? tipIndex
        mnextBlockAfterTip = do
          tipIndex <- mtipIdx
          tipIndexNext <- if tipIndex + 1 < plainBlockchainSz
            then return <! tipIndex + 1
            else Nothing
          plainBlockchain !? tipIndexNext
    (fail <. Text.unpack)
      <! Text.unlines
      <!
        [ "ERROR: " <> name  <> ":"
        , "size of the confirmed blockchain is not the same as initial blockchain"
        , "initial blockchain size = " <> tshow sizeOfInitialBlockchain
        , "confirmed blockchain size = " <> tshow confirmedBlockchainSize
        , "blocks to confirm = " <> tshow configBlocksToConfirmInt
        , "mcurrentConfirmedTip = " <> tshow mcurrentConfirmedTip
        , "currentTime = " <> tshow currentTime
        , "latest unconfirmed tip = " <> tshow munconfirmedTip
        , "last block of the blockchain = " <> tshow lastblock
        , "bitcoin mtipIdx = " <> tshow mtipIdx
        , "bitcoin plainBlockchainSz = " <> tshow plainBlockchainSz
        , "possible next block after tip blockchain = " <> tshow mnextBlockAfterTip
        ] ++ lastRequests
  where
  bitcoinState = Env.bitcoinClientState envState
  sizeOfInitialBlockchain = V.length blockchain
  configBlocksToConfirmInt = fromNatural <! Config.configBlocksToConfirm
    <! Env.config env
  dataSourceState = Env.dataSourceState envState

prop_confirmedBlockchainShouldBeTheSameAsInitialChainButWithNewBranches
  :: Env.State
  -> Env.Environment STM STM IO
  -> Vector Block.BlockHeader
  -> Vector Block.BlockHeader
  -> PropertyM IO ()
prop_confirmedBlockchainShouldBeTheSameAsInitialChainButWithNewBranches
    _envState env _blockchain reorganizedBlocks = do
  mcurrentConfirmedTip <- run <! TVar.readTVarIO <! Env.mCurrentConfirmedTipV env
  currentConfirmedTip <- case mcurrentConfirmedTip of
    Nothing -> fail <! Text.unpack <! Text.unlines
      [ "ERROR: " <> name
      , "there is no confirmed tip"
      ]
    Just some -> return some
  foldM verifyReorganizedBlock () <! V.filter
    (isBlockConfirmed currentConfirmedTip) reorganizedBlocks
  where
  name = "prop_confirmedBlockchainShouldBeTheSameAsInitialChainButWithNewBranches"
  isBlockConfirmed currentConfirmedTip block =
    Block.blockHeaderHeight block <= Block.blockHeaderHeight currentConfirmedTip
  verifyReorganizedBlock () reorganizedBlock = do
    econfirmedBlock <- run <! runAppM name env <! do
      GetBlockHeaderByHeight.getBlockHeaderByHeight
        <! Block.blockHeaderHeight reorganizedBlock
    confirmedBlock <- case econfirmedBlock of
      Left some -> fail <! Text.unpack <! Text.unlines
        [ "ERROR: " <> name
        , "expected a confirmed block, but got:"
        , tshow some
        ]
      Right some -> return some
    let
        isConfirmedBlockTheSameAsReorganized =
          Block.blockHeaderHash reorganizedBlock == Block.blockHeaderHash confirmedBlock
    unless isConfirmedBlockTheSameAsReorganized <! do
      fail <! Text.unpack <! Text.unlines
        [ "ERROR: " <> name  <> ":"
        , "confirmed block is not the same as reorganized block"
        , "confirmed block = " <> tshow confirmedBlock
        , "reorganized block = " <> tshow reorganizedBlock
        ]

prop_witnessingBlockChainFromTheBottomShouldNotWitnessStalledBranches
  :: Profiler.State
  -> SpecWith ()
prop_witnessingBlockChainFromTheBottomShouldNotWitnessStalledBranches
    profilerProdState =
    let name = "prop_witnessingBlockChainFromTheBottomShouldNotWitnessStalledBranches"
    in it (Text.unpack name)
      <! QC.forAll
         ( generatePropConfig (\_ same -> return same))
      <! \( configBlocksToConfirmInt :: Int
          , tipRawInt :: Int
          , reorgzOffset :: [Int]
          ) -> monadicIO <! do
  let
      configBlocksToConfirm = verifyNatural configBlocksToConfirmInt
      tipInt = tipRawInt + configBlocksToConfirmInt
      tip = verifyNatural tipInt
  blockchain <- run <! Bitcoin.generateBlockChain Bitcoin.genesisMediantime 0
    tip
  eblockchainWithReorgs <- run <! Bitcoin.addReorganizations reorgzOffset blockchain
  (blockchainWithReorgs, reorganaizedBlocks) <- case eblockchainWithReorgs of
    Right some -> return some
    Left some -> fail <! Text.unpack <! name <> ": " <> tshow some
  let
      blockchainWithReorgsSz = V.length blockchainWithReorgs
  (envState, env) <- lift <! Env.initConfigProfilerProdState
    name
    (\c -> c { Config.configBlocksToConfirm = configBlocksToConfirm })
    (Just profilerProdState)
    blockchainWithReorgs
  let
      lastblock = blockchainWithReorgs ! (blockchainWithReorgsSz - 1)
  forM_ blockchainWithReorgs <! \blockToDiscover -> do
    run <! do
      Time.setTimeNS (Env.timeState envState)
        (fromIntegral (Block.blockHeaderTimestamp blockToDiscover) * 1_000_000_000)
      STM.atomically <! TVar.writeTVar (Env.requestLogV envState) Seq.Empty
    shouldContinue <- run <! runAppM name env <! do
      Scheduler.iteration
    unless shouldContinue
      <! (fail <. Text.unpack)
      =<< diagInfo
        (name <> ": block discover failed unexpectedly")
        configBlocksToConfirm
        lastblock
        blockchainWithReorgs
        lastblock
        envState
        env
    prop_latestConfirmedBlockShouldBeTheSameAsInBlockchain envState env tip
      (Block.blockHeaderHeight blockToDiscover) blockToDiscover lastblock

  prop_sizeOfConfirmedBlockchainIsTheSameAsInitialBlockChain envState env
    blockchain lastblock

  prop_confirmedBlockchainShouldBeTheSameAsInitialChainButWithNewBranches
    envState env blockchain reorganaizedBlocks

prop_latestConfirmedBlockShouldBeTheSameAsInBlockchain
  :: Env.State
  -> Env.Environment STM STM IO
  -> Natural Int
  -> Block.BlockHeight
  -> Block.BlockHeader
  -> Block.BlockHeader
  -> PropertyM IO ()
prop_latestConfirmedBlockShouldBeTheSameAsInBlockchain _envState env _tip
    currentBlockHeight _currentBlock _lastblock
    | currentBlockHeight <= Config.configBlocksToConfirm (Env.config env) =
      return ()
prop_latestConfirmedBlockShouldBeTheSameAsInBlockchain envState env _tip
    currentBlockHeight currentBlock lastblock =
    do
  currentNS <- run <! TVar.readTVarIO <! Time.nanosecondsV (Env.timeState envState)
  let
      currentTime = currentNS `div` 1_000_000_000
      configBlocksToConfirm = Config.configBlocksToConfirm <! Env.config env
      blockchainWithReorgs = Bitcoin.blockChain <! Env.bitcoinClientState envState
  mcurrentConfirmedTip <- run <! TVar.readTVarIO <! Env.mCurrentConfirmedTipV env
  currentConfirmedTip <- case mcurrentConfirmedTip of
    Nothing -> do
      (fail <. Text.unpack )
        =<< diagInfo
          (name <> ": there is no confirmed tip")
          configBlocksToConfirm
          lastblock
          blockchainWithReorgs
          currentBlock
          envState
          env
    Just some -> return some
  eisHashTheSame <- run <! runAppM name env <! runExceptT <! do
    confirmedBlock <- ExceptT <! GetBlockHeaderByHeight.getBlockHeaderByHeight
      <! Block.blockHeaderHeight currentConfirmedTip
    blockchainBlockHash <- ExceptT <! Bitcoin.getBlockHash
      <! Block.blockHeaderHeight currentConfirmedTip
    blockFromBlockchain <- ExceptT <! Bitcoin.getBlock blockchainBlockHash
    return
      ( Block.blockHeaderHash confirmedBlock == blockchainBlockHash
      , Block.blockHeaderHash confirmedBlock
      , blockFromBlockchain
      )
  case eisHashTheSame of
    Left some -> do
      requestLog <- run <! TVar.readTVarIO <! Env.requestLogV envState
      let
          lastRequests = Seq.viewr requestLog
            !> F.foldl'
              (\acc item-> tshow item:acc)
              []
            !> List.take 5
            !> List.reverse
      fail <! Text.unpack <! Text.unlines <!
        [ "ERROR: " <> name
        , "unexpected error"
        , tshow some
        , "currentConfirmedTip = " <> tshow currentConfirmedTip
        , "unconfirmed block height = " <> tshow currentBlockHeight
        , "current time = " <> tshow currentTime
        ]
        ++ lastRequests
    Right (isHashTheSame, confirmedHash, blockFromBlockchain ) -> do
      unless isHashTheSame <! do
        requestLog <- run <! TVar.readTVarIO <! Env.requestLogV envState
        let
            lastRequests = Seq.viewr requestLog
              !> F.foldl'
                (\acc item-> tshow item:acc)
                []
              !> List.take 5
              !> List.reverse
        fail <! Text.unpack <! Text.unlines <!
          [ "ERROR: " <> name
          , "hash of the confirmed block is not the same as hash in bitcoin client"
          , "currentConfirmedTip = " <> tshow currentConfirmedTip
          , "unconfirmed block height = " <> tshow currentBlockHeight
          , "confirmed block hash = " <> tshow confirmedHash
          , "unconfirmed block = " <> tshow blockFromBlockchain
          , "current time = " <> tshow currentTime
          ]
          ++ lastRequests
  where
  name = "prop_latestConfirmedBlockShouldBeTheSameAsInBlockchain"

prop_witnessingBlockChainFromTheBottomShouldHandleStalledBrancheOverConfigBarrierBy1
  :: Profiler.State
  -> SpecWith ()
prop_witnessingBlockChainFromTheBottomShouldHandleStalledBrancheOverConfigBarrierBy1
    profilerProdState =
    let name = "prop_witnessingBlockChainFromTheBottomShouldHandleStalledBrancheOverConfigBarrierBy1"
    in it (Text.unpack name)
      <! QC.forAll
         ( generatePropConfig addUnsupportedReorganization)
      <! \( configBlocksToConfirmInt :: Int
          , tipRawInt :: Int
          , reorgzOffsets :: [Int]
          ) -> monadicIO <! do
  let
      configBlocksToConfirm = verifyNatural configBlocksToConfirmInt
      tipInt = tipRawInt + configBlocksToConfirmInt
      tip = verifyNatural tipInt
  blockchain <- run <! Bitcoin.generateBlockChain Bitcoin.genesisMediantime 0
    tip
  eblockchainWithReorgs <- run <! Bitcoin.addReorganizations
    reorgzOffsets blockchain
  (blockchainWithReorgs, reorganaizedBlocks) <- case eblockchainWithReorgs of
    Right some -> return some
    Left some -> fail <! Text.unpack <! name <> ": " <> tshow some
  let
      blockchainWithReorgsSz = V.length blockchainWithReorgs
  (envState, env) <- lift <! Env.initConfigProfilerProdState
    name
    (\c -> c { Config.configBlocksToConfirm = configBlocksToConfirm })
    (Just profilerProdState)
    blockchainWithReorgs
  let
      lastblock = blockchainWithReorgs ! (blockchainWithReorgsSz - 1)
  forM_ blockchainWithReorgs <! \blockToDiscover -> do
    run <! do
      Time.setTimeNS (Env.timeState envState)
        (fromIntegral (Block.blockHeaderTimestamp blockToDiscover) * 1_000_000_000)
      STM.atomically <! TVar.writeTVar (Env.requestLogV envState) Seq.Empty
    shouldContinue <- run <! runAppM name env <! do
      Scheduler.iteration
    unless shouldContinue
      <! (fail <. Text.unpack)
      =<< diagInfo
        (name <> ": block discover failed unexpectedly")
        configBlocksToConfirm
        lastblock
        blockchainWithReorgs
        lastblock
        envState
        env
    prop_latestConfirmedBlockShouldBeTheSameAsInBlockchain envState env tip
      (Block.blockHeaderHeight blockToDiscover) blockToDiscover lastblock

  prop_sizeOfConfirmedBlockchainIsTheSameAsInitialBlockChain envState env
    blockchain lastblock

  prop_confirmedBlockchainShouldBeTheSameAsInitialChainButWithNewBranches
    envState env blockchain reorganaizedBlocks
  where
  addUnsupportedReorganization configBlocksToConfirm supportedReorganizations =
   return <! unsupportedReorgzOffset
    where
      newTheLastUnsupportedReorg = configBlocksToConfirm + 1
      (_:otherReorgz) = List.reverse supportedReorganizations
      unsupportedReorgzOffset = List.reverse
        <! newTheLastUnsupportedReorg:otherReorgz

prop_witnessingBlockChainFromTheBottomShouldWitnessStalledBranchesAndNotShutdown
  :: Profiler.State
  -> SpecWith ()
prop_witnessingBlockChainFromTheBottomShouldWitnessStalledBranchesAndNotShutdown
    profilerProdState =
    let name = "prop_witnessingBlockChainFromTheBottomShouldWitnessStalledBranchesAndNotShutdown"
    in it (Text.unpack name)
      <! QC.forAll
         ( generatePropConfig addUnsupportedReorganization)
      <! \( configBlocksToConfirmInt :: Int
          , tipRawInt :: Int
          , reorgzOffsets :: [Int]
          ) -> monadicIO <! do
  let
      configBlocksToConfirm = verifyNatural configBlocksToConfirmInt
      tipInt = tipRawInt + configBlocksToConfirmInt
      tip = verifyNatural tipInt
  blockchain <- run <! Bitcoin.generateBlockChain Bitcoin.genesisMediantime 0
    tip
  eblockchainWithReorgs <- run <! Bitcoin.addReorganizations
    reorgzOffsets blockchain
  (blockchainWithReorgs, _reorganaizedBlocks) <- case eblockchainWithReorgs of
    Right some -> return some
    Left some -> fail <! Text.unpack <! name <> ": " <> tshow some
  let
      blockchainWithReorgsSz = V.length blockchainWithReorgs
  (envState, env) <- lift <! Env.initConfigProfilerProdState
    name
    (\c -> c { Config.configBlocksToConfirm = configBlocksToConfirm })
    (Just profilerProdState)
    blockchainWithReorgs
  let
      lastblock = blockchainWithReorgs ! (blockchainWithReorgsSz - 1)
  forM_ blockchainWithReorgs <! \blockToDiscover -> do
    run <! do
      Time.setTimeNS (Env.timeState envState)
        (fromIntegral (Block.blockHeaderTimestamp blockToDiscover) * 1_000_000_000)
      STM.atomically <! TVar.writeTVar (Env.requestLogV envState) Seq.Empty
    shouldContinue <- run <! runAppM name env <! do
      Scheduler.iteration
    unless shouldContinue
      <! (fail <. Text.unpack)
      =<< diagInfo
        ( name <> ": unsupported reorganization should not cause shutdown"
        )
        configBlocksToConfirm
        lastblock
        blockchainWithReorgs
        lastblock
        envState
        env
  where
  addUnsupportedReorganization configBlocksToConfirm supportedReorganizations = do
    unsupportedBarrierOffset <- QC.choose (2,5)
    let
        newTheLastUnsupportedReorg = configBlocksToConfirm + unsupportedBarrierOffset
        (_:otherReorgz) = List.reverse supportedReorganizations
        unsupportedReorgzOffset = List.reverse
          <! newTheLastUnsupportedReorg:otherReorgz
    return unsupportedReorgzOffset

prop_witnessingBlockChainFromTheBottomShouldWitnessStalledBranchesAndKeepBoth
  :: Profiler.State
  -> SpecWith ()
prop_witnessingBlockChainFromTheBottomShouldWitnessStalledBranchesAndKeepBoth
    profilerProdState =
    it (Text.unpack name)
      <! QC.forAll
         ( generatePropConfig addUnsupportedReorganization)
      <! \( configBlocksToConfirmInt :: Int
          , tipRawInt :: Int
          , reorgzOffsets :: [Int]
          ) -> monadicIO <! do
  let
      configBlocksToConfirm = verifyNatural configBlocksToConfirmInt
      tipInt = tipRawInt + configBlocksToConfirmInt
      tip = verifyNatural tipInt
  blockchain <- run <! Bitcoin.generateBlockChain Bitcoin.genesisMediantime 0
    tip
  eblockchainWithReorgs <- run <! Bitcoin.addReorganizations
    reorgzOffsets blockchain
  (blockchainWithReorgs, reorganaizedBlocks) <- case eblockchainWithReorgs of
    Right some -> return some
    Left some -> fail <! Text.unpack <! name <> ": " <> tshow some
  (envState, env) <- lift <! Env.initConfigProfilerProdState
    name
    (\c -> c { Config.configBlocksToConfirm = configBlocksToConfirm })
    (Just profilerProdState)
    blockchainWithReorgs

  witnessBlocksFromTheBottom envState env tip blockchainWithReorgs
  ensureBothStaleAndBestBranchKept envState env tip blockchain blockchainWithReorgs
    reorgzOffsets reorganaizedBlocks
  where
  name = "prop_witnessingBlockChainFromTheBottomShouldWitnessStalledBranchesAndKeepBoth"
  ensureBothStaleAndBestBranchKept envState env tip blockchain blockchainWithReorgs
      (reorgzOffsets :: [Int]) reorganaizedBlocks  = do
    let
        configBlocksToConfirm = Config.configBlocksToConfirm <! Env.config env
    void <! foldM
      (\restReorganaizedBlocks newBranchSize-> do
        let
            (blocksOfTheNewBranch, restNewBranches) =
              V.splitAt (newBranchSize + 1) restReorganaizedBlocks
        -- run <! Text.putStrLn <! Text.unlines
        --   [ "restReorganaizedBlocks: "
        --     <> tshow
        --       ( V.map
        --         (\x-> (Block.blockHeaderHeight x, Block.blockHeaderHash x) )
        --         restReorganaizedBlocks
        --       )
        --   , "blocksOfTheNewBranch: " <> tshow
        --     ( V.map
        --       (\x-> (Block.blockHeaderHeight x, Block.blockHeaderHash x) )
        --       blocksOfTheNewBranch
        --     )
        --   , "restNewBranches: " <> tshow
        --     ( V.map
        --       (\x-> (Block.blockHeaderHeight x, Block.blockHeaderHash x) )
        --       restNewBranches
        --     )
        --   , "newBranchSize: " <> tshow newBranchSize
        --   , "reorganaizedBlocks: " <> Text.unlines
        --     ( List.map tshow
        --     <! V.toList
        --     <! V.map
        --       (\x-> ( Block.blockHeaderHeight x
        --             , Block.blockHeaderHash x
        --             )
        --       )
        --       reorganaizedBlocks
        --     )
        --   ]
        if newBranchSize > fromNatural configBlocksToConfirm
          then do
            let
                ( partOfNewBranchOfAlreadyConfirmedBlocks
                  , partOfNewBranchOfUnconfirmedBlocks) = V.splitAt
                    (newBranchSize - fromNatural configBlocksToConfirm)
                    blocksOfTheNewBranch
                blocksOfTheOldBranch = V.take newBranchSize
                  <! V.drop
                  ( fromNatural
                    <! Block.blockHeaderHeight
                    <! partOfNewBranchOfAlreadyConfirmedBlocks ! 0
                  )
                  blockchain
                ( partOfOldBranchOfConfirmedBlocks
                  , partOfOldBranchOfUnconfirmedBlocks) = V.splitAt
                    (newBranchSize - fromNatural configBlocksToConfirm)
                    blocksOfTheOldBranch
            bothBlockHashesShouldExist envState env tip blocksOfTheNewBranch
              blockchainWithReorgs
              partOfNewBranchOfAlreadyConfirmedBlocks
              partOfOldBranchOfConfirmedBlocks
            blockHashesShouldExist "newBranchSize > blocks to confirm" envState
              env tip blocksOfTheNewBranch blockchainWithReorgs
              partOfNewBranchOfUnconfirmedBlocks
            blockHashesShouldNotExist "newBranchSize > blocks to confirm"
              envState env tip blocksOfTheNewBranch blockchainWithReorgs
              partOfOldBranchOfUnconfirmedBlocks
          else do
            let
                blocksOfTheOldBranch = V.take newBranchSize
                  <! V.drop
                  ( fromNatural <! Block.blockHeaderHeight
                    <! blocksOfTheNewBranch ! 0
                  )
                  blockchain
            blockHashesShouldExist "newBranchSize <= blocks to confirm"
              envState env tip blocksOfTheNewBranch blockchainWithReorgs
              blocksOfTheNewBranch
            blockHashesShouldNotExist "newBranchSize <= blocks to confirm"
              envState env tip blocksOfTheNewBranch blockchainWithReorgs
              blocksOfTheOldBranch
        return restNewBranches
      )
      ( V.filter
        (checkOnlyConfirmedBlock configBlocksToConfirm tip)
        reorganaizedBlocks
      )
      (reorgzOffsets :: [Int])
  checkOnlyConfirmedBlock configBlockToConfirm tip block =
    Block.blockHeaderHeight block <= (tip - configBlockToConfirm)
  bothBlockHashesShouldExist envState env tip reorganaizedBlocks
      blockchainWithReorgs firstBranch secondBranch = do
    blockHashesShouldExist "both firstBranch" envState env tip reorganaizedBlocks
      blockchainWithReorgs firstBranch
    blockHashesShouldExist "both secondBranch" envState env tip reorganaizedBlocks
      blockchainWithReorgs secondBranch
  blockHashesShouldNotExist
    :: Text
    -> Env.State
    -> Env.Environment STM STM IO
    -> Natural Int
    -> Vector Block.BlockHeader
    -> Vector Block.BlockHeader
    -> Vector Block.BlockHeader
    -> PropertyM IO ()
  blockHashesShouldNotExist prefix envState env tip reorganaizedBlocks
      _blockchainWithReorgs branch = do
    forM_ branch <! \blockShoulNotBeDiscovered -> do
      run <! do
        STM.atomically <! TVar.writeTVar (Env.requestLogV envState) Seq.Empty
      eStoredBlock <- run <! runAppM name env <! do
        GetBlockHeaderByHash.getBlockHeaderByHash
          <! Block.blockHeaderHash blockShoulNotBeDiscovered
      case eStoredBlock of
        Left (BadRequest _) -> return ()
        some -> do
          fail <! Text.unpack <! Text.unlines
            [ name <> ": " <> prefix <> ": block should not exist1 "
              <> tshow (Block.blockHeaderHeight blockShoulNotBeDiscovered)
            , "reason: " <> tshow some
            , "branch: " <> tshow (V.map
              (\x-> (Block.blockHeaderHeight x, Block.blockHeaderHash x))
              branch
              )
            , "reorganaizedBlocks: " <> tshow (V.map Block.blockHeaderHeight reorganaizedBlocks)
            , "tip: " <> tshow tip
            ]
  blockHashesShouldExist prefix envState env _tip reorganaizedBlocks blockchainWithReorgs
      branch = do
    let
        blockchainWithReorgsSz = V.length blockchainWithReorgs
        lastblock = blockchainWithReorgs ! (blockchainWithReorgsSz - 1)
        configBlocksToConfirm = Config.configBlocksToConfirm <! Env.config env
    forM_ branch <! \blockShoulBeDiscovered -> do
      run <! do
        STM.atomically <! TVar.writeTVar (Env.requestLogV envState) Seq.Empty
      eStoredBlock <- run <! runAppM name env <! do
        GetBlockHeaderByHash.getBlockHeaderByHash
          <! Block.blockHeaderHash blockShoulBeDiscovered
      case eStoredBlock of
        Right _ -> return ()
        Left some -> do
          run <! Text.putStrLn <! "blockchainWithReorgs: "
          run <! Text.putStrLn <! Text.unlines
              <! V.toList
              <! V.map (\x-> tshow
                             ( Block.blockHeaderHeight x
                             , Block.blockHeaderTimestamp x
                             , Block.blockHeaderPreviousblockhash x
                             , Block.blockHeaderHash x
                             )
                       )
                       blockchainWithReorgs
          (fail <. Text.unpack)
            =<< diagInfo
              ( Text.unlines
                [ name <> ": " <> prefix <> ": block should exist1 "
                  <> tshow ( Block.blockHeaderHeight blockShoulBeDiscovered
                           , Block.blockHeaderHash blockShoulBeDiscovered
                           )
                , "reason: " <> tshow some
                , "reorganized blocks: " <> tshow reorganaizedBlocks
                ]
              )
              configBlocksToConfirm
              lastblock
              blockchainWithReorgs
              blockShoulBeDiscovered
              envState
              env
  witnessBlocksFromTheBottom envState env _tip blockchainWithReorgs = do
    let
        blockchainWithReorgsSz = V.length blockchainWithReorgs
        lastblock = blockchainWithReorgs ! (blockchainWithReorgsSz - 1)
        configBlocksToConfirm = Config.configBlocksToConfirm <! Env.config env
    forM_ blockchainWithReorgs <! \blockToDiscover -> do
      run <! do
        Time.setTimeNS (Env.timeState envState)
          (fromIntegral (Block.blockHeaderTimestamp blockToDiscover) * 1_000_000_000)
        STM.atomically <! TVar.writeTVar (Env.requestLogV envState) Seq.Empty
      shouldContinue <- run <! runAppM name env <! do
        Scheduler.iteration
      unless shouldContinue
        <! (fail <. Text.unpack)
        =<< diagInfo
          ( name <> ": unsupported reorganization should not cause shutdown"
          )
          configBlocksToConfirm
          lastblock
          blockchainWithReorgs
          lastblock
          envState
          env
  addUnsupportedReorganization configBlocksToConfirm supportedReorganizations = do
    unsupportedBarrierOffset <- QC.choose (2,5)
    let
        newTheLastUnsupportedReorg = configBlocksToConfirm + unsupportedBarrierOffset
        (_:otherReorgz) = List.reverse supportedReorganizations
        unsupportedReorgzOffset = List.reverse <! newTheLastUnsupportedReorg:otherReorgz
    return unsupportedReorgzOffset

