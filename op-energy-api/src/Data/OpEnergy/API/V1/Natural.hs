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
module Data.OpEnergy.API.V1.Natural where

import           Data.Swagger
import           Control.Lens
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Servant.API
import           Data.Scientific( Scientific, toBoundedInteger, scientific)
import qualified Data.Text.Read as TR
import           Database.Persist
import           Database.Persist.Sql
import qualified Data.Serialize             as S
import qualified Data.Text                  as T
import           Numeric

newtype (Ord a, Num a) => Natural a = Natural a
  deriving (Ord, Real, Enum, Integral, Show, Typeable, Eq)
instance (Ord a, Num a) => Num (Natural a) where
  (+) (Natural left) (Natural right) = verifyNatural $! left + right
  (-) (Natural left) (Natural right) = verifyNatural $! left - right
  (*) (Natural left) (Natural right) = verifyNatural $! left * right
  abs v = v
  signum (Natural left) = verifyNatural $ signum left
  fromInteger v = verifyNatural (fromInteger v)

instance ToJSON (Natural Int) where
  toJSON (Natural v) = Number $ scientific (toInteger v) 0
instance FromJSON (Natural Int) where
  parseJSON = withScientific "Natural" $ \v-> return (verifyNaturalScientific v)
instance ToSchema (Natural Int) where
  declareNamedSchema _ = return $ NamedSchema (Just "Natural") $ mempty
    & type_ ?~ SwaggerNumber
    & maximum_ ?~ fromIntegral (maxBound ::Int)
    & minimum_ ?~ 0
instance ToParamSchema (Natural Int) where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerNumber
    & maximum_ ?~ fromIntegral (maxBound ::Int)
    & minimum_ ?~ 0
instance FromHttpApiData (Natural Int) where
  parseUrlPiece t =
    case TR.decimal t of
      Left _ -> Left "Natural: wrong value"
      Right (v, _) -> Right (verifyNaturalInt v)
  parseQueryParam t =
    case TR.decimal t of
      Left _ -> Left "Natural: wrong value"
      Right (v, _)-> Right (verifyNaturalInt v)
instance ToHttpApiData (Natural Int) where
  toUrlPiece (Natural v) = toUrlPiece v
  toQueryParam (Natural v) = toQueryParam v

instance ToJSON (Natural Integer) where
  toJSON (Natural v) = toJSON (showHex v "")
instance FromJSON (Natural Integer) where
  parseJSON = withText "Natural Integer" $ \v-> return (verifyNatural $ fst $ head $ readHex $ T.unpack v)
instance ToSchema (Natural Integer) where
  declareNamedSchema _ = return $ NamedSchema (Just "Natural Integer") $ mempty
    & type_ ?~ SwaggerString

instance PersistField (Natural Int) where
  toPersistValue (Natural s) = toPersistValue s
  fromPersistValue (PersistInt64 s) = Right $! verifyNaturalInt (fromIntegral s) -- TODO
  fromPersistValue _ = Left $ "InputVerification.hs fromPersistValue Natural, expected Text"
instance PersistFieldSql (Natural Int) where
  sqlType _ = SqlInt64

instance PersistField (Natural Integer) where
  toPersistValue (Natural i) = toPersistValue (S.encode i) -- store as bytestring
  fromPersistValue (PersistByteString s) =
    case S.decode s of
      Left some -> Left (T.pack some)
      Right ret -> Right $ verifyNatural ret
  fromPersistValue _ = Left $ "InputVerification.hs fromPersistValue Natural, expected Text"
instance PersistFieldSql (Natural Integer) where
  sqlType _ = SqlBlob


verifyNaturalScientific:: (Integral a, Bounded a) => Scientific -> (Natural a)
verifyNaturalScientific s =
  case toBoundedInteger s of
    Just v -> verifyNatural v
    _ -> error "verifyNaturalScientific: wrong value"

verifyNaturalScientificInt:: Scientific -> (Natural Int)
verifyNaturalScientificInt s =
  case toBoundedInteger s of
    Just v -> verifyNatural v
    _ -> error "verifyNaturalScientific: wrong value"

verifyNatural:: (Ord n, Num n) => n -> (Natural n)
verifyNatural v =
  case () of
    _ | v >= 0 -> Natural v
    _ -> error "verifyNatural: wrong value"

verifyNaturalInt:: Int -> (Natural Int)
verifyNaturalInt v =
  case () of
    _ | v >= 0 -> Natural v
    _ -> error "verifyNatural: wrong value"

fromNatural :: (Ord a, Num a ) => Natural a -> a
fromNatural (Natural v) = v
