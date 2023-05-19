{--
 - This module describes Positive numbers
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.API.V1.Positive where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Scientific( Scientific, toBoundedInteger)
import           Servant.API
import qualified Data.Text.Read as TR

import Data.OpEnergy.API.V1.Natural (Natural(..))

newtype Positive a = Positive Int
  deriving (Ord, Real, Enum, Integral, Show, Generic, Typeable, Eq)

instance Num (Positive a) where
  (+) (Positive left) (Positive right) = verifyPositiveInt $! left + right
  (-) (Positive left) (Positive right) = verifyPositiveInt $! left - right
  (*) (Positive left) (Positive right) = verifyPositiveInt $! left * right
  abs v = v
  signum (Positive left) = verifyPositiveInt $ signum left
  fromInteger v = verifyPositiveInt (fromInteger v)

instance ToJSON (Positive Int)
instance FromJSON (Positive Int) where
  parseJSON = withScientific "Positive" $ \v-> return (verifyPositive v)
instance ToSchema (Positive Int) where
  declareNamedSchema _ = return $ NamedSchema (Just "Positive") $ mempty
    & type_ ?~ SwaggerNumber
    & maximum_ ?~ fromIntegral (maxBound ::Int)
    & minimum_ ?~ 1
instance ToParamSchema (Positive Int) where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerNumber
    & maximum_ ?~ fromIntegral (maxBound ::Int)
    & minimum_ ?~ 1
instance FromHttpApiData (Positive Int) where
  parseUrlPiece t =
    case TR.decimal t of
      Left _ -> Left "Positive: wrong value"
      Right (v, _) -> Right (verifyPositiveInt v)
  parseQueryParam t =
    case TR.decimal t of
      Left _ -> Left "Positive: wrong value"
      Right (v, _)-> Right (verifyPositiveInt v)
instance ToHttpApiData (Positive Int) where
  toUrlPiece (Positive v) = toUrlPiece v
  toQueryParam (Positive v) = toQueryParam v

verifyPositiveInt:: Int -> Positive a
verifyPositiveInt v =
  if v > 0
  then Positive v
  else error "verifyPositiveInt: wrong value"

verifyPositive:: Scientific -> Positive a
verifyPositive s =
  case toBoundedInteger s of
    Just v -> verifyPositiveInt v
    _  -> error "verifyPositive: wrong value"

fromPositive :: Positive a -> Int
fromPositive (Positive v) = v

-- | Like Positive, but defines type-level parameter of the minimum value
newtype Positive2 a = Positive2
  { unPositive2 :: Int
  }
  deriving (Ord, Real, Enum, Integral, Show, Generic, Typeable, Eq)

minimumPositive2 :: Positive2 Int
minimumPositive2 = Positive2 2

instance Num (Positive2 a) where
  (+) (Positive2 left) (Positive2 right) = verifyPositive2Int $! left + right
  (-) (Positive2 left) (Positive2 right) = verifyPositive2Int $! left - right
  (*) (Positive2 left) (Positive2 right) = verifyPositive2Int $! left * right
  abs v = v
  signum (Positive2 left) = verifyPositive2Int $ signum left
  fromInteger v = verifyPositive2Int (fromInteger v)

instance ToJSON (Positive2 Int) where
  toJSON (Positive2 v) = toJSON v
instance FromJSON (Positive2 Int) where
  parseJSON = withScientific "Positive2" $ \v-> return (verifyPositive2 v)
instance ToSchema (Positive2 Int) where
  declareNamedSchema _ = return $ NamedSchema (Just "Positive2") $ mempty
    & type_ ?~ SwaggerNumber
    & maximum_ ?~ fromIntegral (maxBound ::Int)
    & minimum_ ?~ fromIntegral (unPositive2 minimumPositive2)
instance ToParamSchema (Positive2 Int) where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerNumber
    & maximum_ ?~ fromIntegral (maxBound ::Int)
    & minimum_ ?~ fromIntegral (unPositive2 minimumPositive2)
instance FromHttpApiData (Positive2 Int) where
  parseUrlPiece t =
    case TR.decimal t of
      Left _ -> Left "Positive2: wrong value"
      Right (v, _) -> Right (verifyPositive2Int v)
  parseQueryParam t =
    case TR.decimal t of
      Left _ -> Left "Positive2: wrong value"
      Right (v, _)-> Right (verifyPositive2Int v)
instance ToHttpApiData (Positive2 Int) where
  toUrlPiece (Positive2 v) = toUrlPiece v
  toQueryParam (Positive2 v) = toQueryParam v

verifyPositive2Int:: Int -> Positive2 a
verifyPositive2Int v =
  if v >= unPositive2 minimumPositive2
  then Positive2 v
  else error $ "verifyPositive2Int: " ++ show v ++ " wrong value"

verifyPositive2:: Scientific -> Positive2 a
verifyPositive2 s =
  case toBoundedInteger s of
    Just v -> verifyPositive2Int v
    _  -> error "verifyPositive2: wrong value"

positiveFromPositive2 :: Positive2 a -> Positive a
positiveFromPositive2 (Positive2 v) = Positive v

naturalFromPositive :: Positive a-> Natural Int
naturalFromPositive (Positive v) = Natural v
