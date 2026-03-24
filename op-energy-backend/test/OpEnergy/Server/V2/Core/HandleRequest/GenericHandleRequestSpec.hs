module OpEnergy.Server.V2.Core.HandleRequest.GenericHandleRequestSpec
  ( spec
  ) where

import qualified Data.Text as Text
import           Control.Monad( void, unless, when)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import           Data.Foldable
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Sequence as Seq
import           Data.Vector ((!))
import           Flow

import           Data.Text.Show

import           Test.Hspec
import           Test.QuickCheck.Monadic
import qualified Test.QuickCheck as QC

-- import           Control.Monad.Trans.Except(ExceptT(..), runExceptT)

import           Data.OpEnergy.API.V1.Natural(verifyNatural)
import qualified Data.OpEnergy.API.V1.Block as Block

import           OpEnergy.Server.V2.Core.Call
import           OpEnergy.Server.V2.Core.App( runAppM)
import qualified OpEnergy.Server.V2.Core.Request as Request
import qualified OpEnergy.Server.V2.Core.HandleRequest as HandleRequest
import qualified OpEnergy.Server.V2.Environment.Test as Env
import qualified OpEnergy.Server.V2.Environment.Request as EnvRequest
import qualified OpEnergy.Server.V2.Environment.Profiler.Request as Profiler
import qualified OpEnergy.Server.V2.Environment.Logger.Request as Logger
import qualified OpEnergy.Server.V2.Environment.BitcoinClient.Test as Bitcoin
import qualified OpEnergy.Server.V2.Environment.Time.Manual as Time
import qualified OpEnergy.Server.V2.Environment.DataSource.Request as DataSource
import qualified OpEnergy.Server.V2.Core.SchedulerThread as Scheduler

spec :: Spec
spec = describe "handleRequest-generic" <! do
  tip <- runIO <! QC.generate <! QC.choose (0,10000)
  blockchain <- runIO <! Bitcoin.generateBlockChain Bitcoin.genesisMediantime 0
    (verifyNatural tip)
  (envState, env) <- runIO <! Env.init blockchain
  let
      lastblock = blockchain ! tip
  runIO <! Time.setTimeNS (Env.timeState envState)
    (fromIntegral (Block.blockHeaderTimestamp lastblock) * 1_000_000_000)
  shouldContinue <- runIO <! runAppM "test" env <! do
    Scheduler.iteration
  it "Scheduler.iteration should not fail" <! do
    shouldContinue `shouldBe` True
  let
      cleanRequestLog = runIO <! STM.atomically <! TVar.writeTVar (Env.requestLogV envState) Seq.Empty
  cleanRequestLog


  it "allPossibleRequests should be > 0" <! do
    List.length Request.allPossibleRequests `shouldSatisfy` (>0)
  forM_ Request.allPossibleRequests <! \emptyRequest -> do
    (requestAllowedToNotToLog, request) <- do
      case emptyRequest of
        Request.Ping _ -> return ( True, emptyRequest)
        Request.GetBlockByHeight _ -> do
          mheight <- runIO <! do
            (mheight0 :: Maybe Int) <- QC.generate QC.arbitrary
            case mheight0 of
              Nothing -> return Nothing
              Just _ -> do
                (Just <. verifyNatural) <$> (QC.generate <! QC.choose (0, tip))
          return (False, Request.GetBlockByHeight (Call mheight Nothing))
        -- _ -> False
    let
        requestT = tshow request |> Text.takeWhile Char.isAlphaNum

    it ( Text.unpack requestT <> ": response constructor should always match request") <! do
      response <- runAppM "test" env <! do
        HandleRequest.handleRequest request
      (request == response) `shouldBe'` True <! tshow request

    it (Text.unpack requestT <> ": any request should profile function at least ones") <! do
      void <! runAppM "test" env <! do
        void <! HandleRequest.handleRequest emptyRequest
      requestLog <- STM.atomically <! TVar.readTVar (Env.requestLogV envState)
      let
          profilesExist = foldl' (\acc logItem ->
              case logItem of
                EnvRequest.Profiler (Profiler.Profile (Cast (Just callstack)))
                  | callstack == requestT -> True
                _ -> acc
            )
            False
            requestLog
      profilesExist `shouldBe'` True <! tshow request

    unless requestAllowedToNotToLog <! do
      it (Text.unpack requestT <> ": any request should log input at least ones") <! do
        void <! runAppM "test" env <! do
          void <! HandleRequest.handleRequest request
        requestLog <- STM.atomically <! TVar.readTVar (Env.requestLogV envState)
        let
            loggerExist = foldl' (\acc logItem ->
                case logItem of
                  EnvRequest.Logger (Logger.LogInput (Cast (Just _))) -> True
                  _ -> acc
              )
              False
              requestLog
        loggerExist `shouldBe'` True <! tshow request

  it "no request should write to data source"
      <! QC.forAll (Request.generateRequestUpToHeight 0 tip)
      <! \request-> monadicIO <! do
    run <! runAppM "test" env <! do
      void <! HandleRequest.handleRequest request
    requestLog <- run <! STM.atomically <! TVar.readTVar (Env.requestLogV envState)
    let
        dsWritesExist = foldl' (\acc logItem ->
            case logItem of
              EnvRequest.DataSource (DataSource.WriteRequest _) -> True
              _ -> acc
          )
          False
          requestLog
    when dsWritesExist
      <! fail <! Text.unpack <! "Failure: " <> tshow request

  where
  shouldBe' expected actual msg = do
    unless (expected == actual) <!
      expectationFailure <! Text.unpack <! "Failure: " <> msg

