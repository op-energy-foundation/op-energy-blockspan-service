module OpEnergy.API.V1.HashSpec
  ( spec
  ) where

import qualified Data.Aeson as Aeson
import qualified Database.Persist as Persist
import qualified Servant.API as API
import           Test.Hspec
import           Test.QuickCheck

import           Data.OpEnergy.API.V1.Hash

spec :: Spec
spec = do
  describe "Hash serialization" $ do
    it "from/to JSON should be the same" $
      property $ \(x::Hash) -> Aeson.eitherDecode (Aeson.encode x) == Right x
    it "from/to Persist should be the same" $
      property $ \(x::Hash) -> Persist.fromPersistValue (Persist.toPersistValue x) == Right x
    it "from/to HTTP should be the same" $
      property $ \(x::Hash) -> API.parseUrlPiece (API.toUrlPiece x) == Right x
    it "from/to HTTP should be the same" $
      property $ \(x::Hash) -> API.parseQueryParam (API.toQueryParam x) == Right x
