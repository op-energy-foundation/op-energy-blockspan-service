module OpEnergy.Server.V2.Core.SchedulerThread.Iteration.ConfirmationSpec
  ( spec
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Control.Monad( forM_, unless)
import           Control.Monad.Trans( lift)
import           Control.Concurrent.STM(STM)
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Data.Map.Strict as Map
import           Data.Vector (Vector, (!))
import qualified Data.List as List
import           Flow

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import qualified Test.QuickCheck as QC

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
import qualified OpEnergy.Server.V1.Config as Config

spec :: Spec
spec = describe "scheduler.iteration.confirmations" $ do

  it "confirmation blocks rules"
      <! property <! \(_some::Int) -> monadicIO <! do
    configBlocksToConfirm <- run <! fmap verifyNatural <! QC.generate
      <! QC.choose (0, 1000)
    tip <- run <! fmap verifyNatural <! QC.generate
      <! QC.choose (fromNatural configBlocksToConfirm + 2, 5000)
    blockchain <- run <! Bitcoin.generateBlockChain Bitcoin.genesisMediantime 0
      tip
    (envState, env) <- lift <! Env.initConfig
      (\c -> c {Config.configBlocksToConfirm = configBlocksToConfirm})
      blockchain

    prop_first_unconfirmed_blocks_should_not_be_stored configBlocksToConfirm
      tip blockchain envState env

    prop_first_confirmed_block_should_trigger_first_block_to_be_stored configBlocksToConfirm
      tip blockchain envState env

    prop_evaluating_all_blockchain_persists_only_confirmed_blocks configBlocksToConfirm
      tip blockchain envState env

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
  mcurrentConfirmedTip <- TVar.readTVarIO <! Env.mcurrentTipV env
  blocks <- TVar.readTVarIO <! DataSource.blocksV dataSourceState
  mtipBlock <- TVar.readTVarIO <! Bitcoin.mtipBlockV
    <! Env.bitcoinClientState envState
  let
      mchainTipBlock = case mtipBlock of
        Nothing -> Nothing
        Just chainTip -> Just <! blockchain ! chainTip
  return <! Text.unlines
    [ description
    , "size = " <> tshow ( Map.size blocks)
    , "blockHeight = " <> tshow blockHeight
    , "configBlocksToConfirm = " <> tshow configBlocksToConfirm
    , "bitcoin tip max = " <> tshow tip
    , "mcurrentConfirmedTip = " <> tshow mcurrentConfirmedTip
    , "mchainTipBlock = " <> tshow mchainTipBlock
    , "time = " <> tshow (Block.blockHeaderMediantime block)
    , "block = " <> tshow block
    ]
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
    run <! Time.setTimeNS (Env.timeState envState)
      (fromIntegral (Block.blockHeaderTimestamp block) * 1_000_000_000)
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
  run <! Time.setTimeNS (Env.timeState envState)
    (fromIntegral (Block.blockHeaderTimestamp block) * 1_000_000_000)
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
    run <! Time.setTimeNS (Env.timeState envState)
      (fromIntegral (Block.blockHeaderTimestamp block) * 1_000_000_000)
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

