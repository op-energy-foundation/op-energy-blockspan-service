{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Core.Request
  ( Request(..)
  , allPossibleRequests
  , generateRequestUpToHeight
  ) where

import qualified Data.List as List
import           Flow

import           Test.QuickCheck (Gen, Arbitrary(..))
import qualified Test.QuickCheck as QC

import           Data.OpEnergy.API.V1.Block(BlockHeight, BlockHeader(..))
import           Data.OpEnergy.API.V1.Natural( verifyNatural)

import           OpEnergy.Server.V2.Core.Call(Call0(..), Call(..))


data Request
  = GetBlockByHeight (Call BlockHeight BlockHeader) -- keep this request as the first
  -- put all the rest requests below this line
  -- put all the rest request above this line
  | Ping (Call0 () ) -- keep this request as the last
  deriving (Show)
-- this should be the last request
maxRequest :: Request
maxRequest = Ping (Call0 Nothing)

instance Bounded Request where
  minBound = GetBlockByHeight (Call Nothing Nothing)
  maxBound = maxRequest

allPossibleRequests :: [Request]
allPossibleRequests = List.map toEnum ([ 0 .. fromEnum maxRequest ] :: [Int])

instance Enum Request where
  toEnum 0 = GetBlockByHeight (Call Nothing Nothing)
  toEnum 1 = Ping (Call0 Nothing) -- this line should be the last. Increase value accordingly
  toEnum v = error ("toEnum Request: unsupported value" ++ show v)
  fromEnum (GetBlockByHeight _ ) = 0 -- should be the first line
  fromEnum (Ping _ ) = 1 -- this line should be the last. Increase value accordingly

-- WARNING: we check only constructors, not the arguments!
instance Eq Request where
  l == r = fromEnum l == fromEnum r

-- WARNING: we check only constructors, not the arguments!
instance Ord Request where
  compare l r = compare (fromEnum l) (fromEnum r)

generateRequestUpToHeight
  :: Int
  -> Int
  -> Gen Request
generateRequestUpToHeight minHeight maxHeight = do
  emptyRequest <- toEnum <$> QC.choose (0, fromEnum maxRequest)
  case emptyRequest of
    Ping _ -> return emptyRequest
    GetBlockByHeight _ -> do
      (mheight0 :: Maybe Int) <- QC.arbitrary
      mheight <- case mheight0 of
        Nothing -> return Nothing
        Just _ -> (Just <. verifyNatural) <$> QC.choose (minHeight, maxHeight)
      return <! GetBlockByHeight (Call mheight Nothing)

instance Arbitrary Request where
  arbitrary = do
    tip <- QC.choose (1, 5000)
    generateRequestUpToHeight 1 tip

