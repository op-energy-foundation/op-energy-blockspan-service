module OpEnergy.Server.V2.Core.HandleRequest.Ping.PingSpec
  ( spec
  ) where

import           Control.Monad.Trans(lift)
import           Flow

import           Data.Text.Show

import           Test.Hspec

import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import qualified Test.QuickCheck as QC

import           Control.Monad.Trans.Except(ExceptT(..), runExceptT, throwE)

import           Data.OpEnergy.API.V1.Natural( verifyNatural)
import           OpEnergy.Server.V2.Core.Call
import           OpEnergy.Server.V2.Core.App( runAppM)
import qualified OpEnergy.Server.V2.Core.Request as Request
import qualified OpEnergy.Server.V2.Core.HandleRequest as HandleRequest
import qualified OpEnergy.Server.V2.Environment.Test as Env
import qualified OpEnergy.Server.V2.Environment.BitcoinClient.Test as Bitcoin

spec :: Spec
spec = describe "Ping" $ do

  tip <- runIO <| QC.generate <| QC.choose (0,10000)
  blockchain <- runIO <| Bitcoin.generateBlockChain Bitcoin.genesisMediantime 0
    (verifyNatural tip)
  (_, env) <- runIO <| Env.init "Ping" blockchain
  it "should never fail" <| property <| \(_some::Int) -> monadicIO <| do
    eresponse <- run <| runAppM "test" env <| runExceptT <| do
      someResponse <- lift <| HandleRequest.handleRequest <| Request.Ping (Call0 Nothing)
      eresponse <- do
        case someResponse of
          Request.Ping (Call0 (Just some))-> return some
          other -> throwE <| Internal <| "expected Ping (Call0 (Just (Right ()))) got " <> tshow other
      ExceptT $ return eresponse
    assert ( either (const False) (const True) eresponse)



