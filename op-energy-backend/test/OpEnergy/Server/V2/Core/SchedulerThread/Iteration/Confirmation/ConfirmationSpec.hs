module OpEnergy.Server.V2.Core.SchedulerThread.Iteration.Confirmation.ConfirmationSpec
  ( spec
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Control.Monad( forM_, unless)
import           Control.Monad.Trans( lift)
import           Control.Concurrent.STM(STM)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Data.Map.Strict as Map
import           Data.Vector (Vector, (!) )
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

import           OpEnergy.Server.V2.Core.App( runAppM)
import qualified OpEnergy.Server.V2.Core.SchedulerThread as Scheduler
import qualified OpEnergy.Server.V2.Environment as Env
import qualified OpEnergy.Server.V2.Environment.Test as Env
import qualified OpEnergy.Server.V2.Environment.BitcoinClient.Test as Bitcoin
import qualified OpEnergy.Server.V2.Environment.Time.Manual as Time
import qualified OpEnergy.Server.V2.Environment.DataSource.Test as DataSource
import qualified OpEnergy.Server.V2.Environment.Request as EnvRequest
import qualified OpEnergy.Server.V1.Config as Config

spec :: Spec
spec = do
  describe "scheduler.iteration.confirmations" $ do

    it "confirmation blocks rules"
        <! property <! \(_some::Int) -> monadicIO <! do
      configBlocksToConfirm <- run <! fmap verifyNatural <! QC.generate
        <! QC.choose (0, 50)
      tip <- run <! fmap verifyNatural <! QC.generate
        <! QC.choose (fromNatural configBlocksToConfirm + 2, 100)
      blockchain <- run <! Bitcoin.generateBlockChain Bitcoin.genesisMediantime 0
        tip
      (envState, env) <- lift <! Env.initConfigProfilerProdState
        "scheduler.iteration.confirmations"
        (\c -> c {Config.configBlocksToConfirm = configBlocksToConfirm})
        Nothing -- (Just profilerProdState)
        blockchain

      prop_first_unconfirmed_blocks_should_not_be_stored configBlocksToConfirm
        tip blockchain envState env

      prop_first_confirmed_block_should_trigger_first_block_to_be_stored configBlocksToConfirm
        tip blockchain envState env

      prop_evaluating_all_blockchain_persists_only_confirmed_blocks configBlocksToConfirm
        tip blockchain envState env

    -- when we use production profiler, we can output stats for benchmarks and etc
    -- it "should log and clear" <! do
    --   Text.putStrLn =<< Profiler.metricsSerialize
    --   P.unregisterAll


diagInfo
  :: Text
  -> Natural Int
  -> Natural Int
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
          (\acc item-> case item of
            EnvRequest.Profiler _ -> acc
            _ -> (tshow item):acc
          )
          []
        !> List.take 20
        !> List.reverse
      mchainTipBlock = case mtipBlock of
        Nothing -> Nothing
        Just chainTip -> Just <! blockchain ! chainTip
  return <! Text.unlines
    <!
      [ description
      , "size = " <> tshow ( Map.size blocks)
      , "blockHeight = " <> tshow blockHeight
      , "configBlocksToConfirm = " <> tshow configBlocksToConfirm
      , "bitcoin tip max = " <> tshow tip
      , "mcurrentConfirmedTip = " <> tshow mcurrentConfirmedTip
      , "mWitnessedUnconfirmedTip = " <> tshow mWitnessedUnconfirmedTip
      , "mchainTipBlock = " <> tshow mchainTipBlock
      , "block = " <> tshow block
      ]
      ++ lastRequests
  where
    dataSourceState = Env.dataSourceState envState
    blockHeight = Block.blockHeaderHeight block

prop_first_unconfirmed_blocks_should_not_be_stored
  :: Natural Int
  -> Natural Int
  -> Vector Block.BlockHeader
  -> Env.State
  -> Env.Environment STM STM IO
  -> PropertyM IO ()
prop_first_unconfirmed_blocks_should_not_be_stored configBlocksToConfirm
    tip blockchain envState env =
    let name = "prop_first_unconfirmed_blocks_should_not_be_stored"
    in do
  let
      lastBlockHeightNotTriggersConfirmation =
        fromNatural configBlocksToConfirm - 1
  forM_ [ 0 .. lastBlockHeightNotTriggersConfirmation ] <! \blockHeight-> do
    let
        block = blockchain ! blockHeight
    run <! do
      Time.setTimeNS (Env.timeState envState)
        (fromIntegral (Block.blockHeaderTimestamp block) * 1_000_000_000)
      STM.atomically <! TVar.writeTVar (Env.requestLogV envState) Seq.Empty
    shouldContinue <- run <! runAppM "test" env <! do
      Scheduler.iteration
    unless shouldContinue
      <! (fail <. Text.unpack)
      =<< diagInfo
        (name <> ": " <> "iteration's fail unexpected")
        configBlocksToConfirm
        tip
        blockchain
        block
        envState
        env
    let
        dataSourceState = Env.dataSourceState envState
    blocks <- run <! TVar.readTVarIO <! DataSource.blocksV dataSourceState
    let
        noBlocksShouldHaveBeenPersisted = Map.size blocks < 1
    unless noBlocksShouldHaveBeenPersisted
      <! (fail <. Text.unpack)
      =<< diagInfo
        (name <> ": " <> "data source stored block, which is not unexpected")
        configBlocksToConfirm
        tip
        blockchain
        block
        envState
        env

prop_first_confirmed_block_should_trigger_first_block_to_be_stored
  :: Natural Int
  -> Natural Int
  -> Vector Block.BlockHeader
  -> Env.State
  -> Env.Environment STM STM IO
  -> PropertyM IO ()
prop_first_confirmed_block_should_trigger_first_block_to_be_stored
    configBlocksToConfirm
    tip
    blockchain
    envState
    env =
    let name = "prop_first_confirmed_block_should_trigger_first_block_to_be_stored"
    in do
  let
      theFirstUnconfirmedBlockTriggersConfirmation = configBlocksToConfirm
      block = blockchain ! fromNatural theFirstUnconfirmedBlockTriggersConfirmation
  run <! do
    Time.setTimeNS (Env.timeState envState)
      (fromIntegral (Block.blockHeaderTimestamp block) * 1_000_000_000)
    STM.atomically <! TVar.writeTVar (Env.requestLogV envState) Seq.Empty
  shouldContinue <- run <! runAppM "test" env <! do
    Scheduler.iteration
  unless shouldContinue
    <! (fail <. Text.unpack)
    =<< diagInfo
      (name <> ": iteration's fail unexpected")
      configBlocksToConfirm
      tip
      blockchain
      block
      envState
      env
  let
      dataSourceState = Env.dataSourceState envState
  blocks <- run <! TVar.readTVarIO <! DataSource.blocksV dataSourceState
  let
      firstBlockShouldHaveBeenStored = Map.size blocks == 1
  unless firstBlockShouldHaveBeenStored
    <! (fail <. Text.unpack)
    =<< diagInfo
      (name <> ": data source haven't stored block, which is not unexpected")
      configBlocksToConfirm
      tip
      blockchain
      block
      envState
      env
  let
      (first:_) = Map.elems blocks
      firstConfirmedBlockShouldBeWithHeight0 =
        Block.blockHeaderHeight first == 0
  unless firstConfirmedBlockShouldBeWithHeight0
    <! (fail <. Text.unpack)
    =<< diagInfo
      (name <> ": ")
      configBlocksToConfirm
      tip
      blockchain
      first
      envState
      env

prop_evaluating_all_blockchain_persists_only_confirmed_blocks
  :: Natural Int
  -> Natural Int
  -> Vector Block.BlockHeader
  -> Env.State
  -> Env.Environment STM STM IO
  -> PropertyM IO ()
prop_evaluating_all_blockchain_persists_only_confirmed_blocks
    configBlocksToConfirm
    tip
    blockchain
    envState
    env =
    let name = "prop_evaluating_all_blockchain_persists_only_confirmed_blocks"
    in do
  let
      restBlockchainPart = fromIntegral configBlocksToConfirm + 1
  forM_ [ restBlockchainPart .. fromIntegral tip ] <! \blockHeight-> do
    let
        block = blockchain ! blockHeight
    run <! do
      Time.setTimeNS (Env.timeState envState)
        (fromIntegral (Block.blockHeaderTimestamp block) * 1_000_000_000)
      STM.atomically <! TVar.writeTVar (Env.requestLogV envState) Seq.Empty
    shouldContinue <- run <! runAppM "test" env <! do
      Scheduler.iteration
    unless shouldContinue
      <! (fail <. Text.unpack)
      =<< diagInfo
        (name <> ": " <> "iteration's fail unexpected")
        configBlocksToConfirm
        tip
        blockchain
        block
        envState
        env
    let
        dataSourceState = Env.dataSourceState envState
    storedBlocks <- run <! TVar.readTVarIO <! DataSource.blocksV dataSourceState
    let
        blocksShouldHaveBeenPersisted =
          Map.size storedBlocks
            == List.length [ 0 .. blockHeight - fromIntegral configBlocksToConfirm ]
    unless blocksShouldHaveBeenPersisted
      <! (fail <. Text.unpack)
      =<< diagInfo
        (name <> ": " <> "size blocks != blockHeight - configBlocksToConfirm, which is not unexpected")
        configBlocksToConfirm
        tip
        blockchain
        block
        envState
        env
    -- run <! Text.putStrLn =<< Profiler.metricsSerialize


