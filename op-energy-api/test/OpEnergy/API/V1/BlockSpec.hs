module OpEnergy.API.V1.BlockSpec
  ( spec
  ) where

import qualified Data.Aeson as Aeson
import qualified Database.Persist as Persist
import           Test.Hspec
import           Test.QuickCheck

import           Data.OpEnergy.API.V1.Block

spec :: Spec
spec = do
  blockHeaderSpecs
  blockSpanSpecs

blockHeaderSpecs :: Spec
blockHeaderSpecs = do
  describe "BlockHeader serialization" $ do
    it "from/to JSON should be the same" $
      property $ \(x::BlockHeader) -> Aeson.eitherDecode (Aeson.encode x) == Right x
    it "from/to Persist should be the same" $
      property $ \(x::BlockHeader) -> Persist.fromPersistValue (Persist.toPersistValue x) == Right x

blockSpanSpecs :: Spec
blockSpanSpecs = do
  describe "BlockSpan serialization" $ do
    it "from/to JSON should be the same" $
      property $ \(x::BlockSpan) -> Aeson.eitherDecode (Aeson.encode x) == Right x
