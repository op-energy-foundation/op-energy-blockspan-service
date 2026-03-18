module OpEnergy.Server.V2.Core.HandleRequest.GenericHandleRequestSpec
  ( spec
  ) where

import qualified Data.Text as Text
import           Control.Monad( void, unless)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import           Data.Foldable
import qualified Data.Char as Char
import           Flow

import           Data.Text.Show

import           Test.Hspec

-- import           Test.QuickCheck
-- import           Test.QuickCheck.Monadic
import qualified Test.QuickCheck as QC

-- import           Control.Monad.Trans.Except(ExceptT(..), runExceptT)

import           Data.OpEnergy.API.V1.Natural(verifyNatural)
import           OpEnergy.Server.V2.Core.Call
import           OpEnergy.Server.V2.Core.App( runAppM)
import qualified OpEnergy.Server.V2.Core.Request as Request
import qualified OpEnergy.Server.V2.Core.HandleRequest as HandleRequest
import qualified OpEnergy.Server.V2.Environment.Test as Env
import qualified OpEnergy.Server.V2.Environment.Request as EnvRequest
import qualified OpEnergy.Server.V2.Environment.Profiler.Request as Profiler
import qualified OpEnergy.Server.V2.Environment.Logger.Request as Logger
import qualified OpEnergy.Server.V2.Environment.BitcoinClient.Test as Bitcoin

spec :: Spec
spec = describe "handleRequest-generic" $ do
  forM_ Request.allPossibleRequests <| \request -> do
    let
        requestT = tshow request |> Text.takeWhile Char.isAlphaNum

    it ( Text.unpack requestT <> ": response constructor should always match request") $ do
      tip <- QC.generate <| QC.choose (0,10000)
      blockchain <- Bitcoin.generateBlockChain Bitcoin.genesisMediantime 0
        (verifyNatural tip)
      (_, env) <- Env.init blockchain
      response <- runAppM "test" env <| do
        HandleRequest.handleRequest request
      (request == response) `shouldBe` True

    it (Text.unpack requestT <> ": any request should profile function at least ones") $ do
      tip <- QC.generate <| QC.choose (0,10000)
      blockchain <- Bitcoin.generateBlockChain Bitcoin.genesisMediantime 0
        (verifyNatural tip)
      (envState, env) <- Env.init blockchain
      void <| runAppM "test" env <| do
        void <| HandleRequest.handleRequest request
      requestLog <- STM.atomically $ TVar.readTVar (Env.requestLogV envState)
      let
          profilesExist = foldl' (\acc logItem ->
              case logItem of
                EnvRequest.Profiler (Profiler.Profile (Cast (Just callstack)))
                  | callstack == requestT -> True
                _ -> acc
            )
            False
            requestLog
      profilesExist `shouldBe` True

    let
        requestAllowedToNotToLog =
          case request of
            Request.Ping _ -> True
            -- _ -> False
    unless requestAllowedToNotToLog <| do
      it (Text.unpack requestT <> ": any request should log input at least ones") $ do
        tip <- QC.generate <| QC.choose (0,10000)
        blockchain <- Bitcoin.generateBlockChain Bitcoin.genesisMediantime 0
          (verifyNatural tip)
        (envState, env) <- Env.init blockchain
        void <| runAppM "test" env <| do
          void <| HandleRequest.handleRequest request
        requestLog <- STM.atomically $ TVar.readTVar (Env.requestLogV envState)
        let
            loggerExist = foldl' (\acc logItem ->
                case logItem of
                  EnvRequest.Logger (Logger.LogInput (Cast (Just _))) -> True
                  _ -> acc
              )
              False
              requestLog
        loggerExist `shouldBe` True


