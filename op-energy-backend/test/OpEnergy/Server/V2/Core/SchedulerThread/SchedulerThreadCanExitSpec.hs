module OpEnergy.Server.V2.Core.SchedulerThread.SchedulerThreadCanExitSpec
  ( spec
  ) where

import           Control.Monad( void)
import           Control.Monad.Trans( lift)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import           Data.Vector((!))
import qualified Data.Vector as V

import           Flow

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import qualified Test.QuickCheck as QC

import           Data.OpEnergy.API.V1.Natural( verifyNatural)
import           Data.OpEnergy.API.V1.Block( BlockHeader(..))
import           OpEnergy.Server.V2.Core.App( runAppM)
import qualified OpEnergy.Server.V2.Core.SchedulerThread as Scheduler
import qualified OpEnergy.Server.V2.Environment.Test as Env
import qualified OpEnergy.Server.V2.Environment as Env
import qualified OpEnergy.Server.V2.Environment.BitcoinClient.Test as Bitcoin
import qualified OpEnergy.Server.V2.Environment.Time.Manual as Time

spec :: Spec
spec = describe "scheduler.iteration" $ do

  it "should not exit without shutdown request"
      <| property <| \(_some::Int) -> monadicIO <| do
    tip <- run <| QC.generate <| QC.choose (0,10000)
    blockchain <- run <| Bitcoin.generateBlockChain Bitcoin.genesisMediantime 0
      (verifyNatural tip)
    (envState, env) <- lift <| Env.init blockchain
    run <| Time.setTimeNS (Env.timeState envState)
      (fromIntegral Bitcoin.genesisMediantime * 1_000_000_000)
    shouldContinue <- run <| runAppM "test" env <| do
      Scheduler.iteration
    assert shouldContinue

  it "should exit on shutdown request"
      <| property <| \(_some::Int) -> monadicIO <| do
    tip <- run <| QC.generate <| QC.choose (0,10000)
    blockchain <- run <| Bitcoin.generateBlockChain Bitcoin.genesisMediantime 0
      (verifyNatural tip)
    (_, env) <- lift <| Env.init blockchain
    void <| lift <| STM.atomically
      <| TVar.writeTVar (Env.shutdownRequestedV env) True
    shouldContinue <- run <| runAppM "test" env <| do
      Scheduler.iteration
    assert ( not shouldContinue )

  it "should exit on when bitcoin node client fails"
      <| property <| \(_some::Int) -> monadicIO <| do
    (_, env) <- lift <| Env.init V.empty
    shouldContinue <- run <| runAppM "test" env <| do
      Scheduler.iteration
    assert ( not shouldContinue )

  it "should not exit on when bitcoin node isn't fails"
      <| property <| \(_some::Int) -> monadicIO <| do
    blockchain <- run <| Bitcoin.generateBlockChain Bitcoin.genesisMediantime 0
      1
    (envState, env) <- lift <| Env.init blockchain
    let
        setTimeOfDiscoveryOfTheGenesisBlock = do
          let
              firstBlock = blockchain ! 0
          run <| Time.setTimeNS (Env.timeState envState)
            (fromIntegral (blockHeaderTimestamp firstBlock) * 1_000_000_000)
    setTimeOfDiscoveryOfTheGenesisBlock
    shouldContinue <- run <| runAppM "test" env <| do
      Scheduler.iteration
    assert shouldContinue


