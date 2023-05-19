{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances          #-}
module Data.OpEnergy.API.V1.Hash where

import           Data.Text                  (Text)
import qualified Data.Text as               T
import           Data.ByteString.Short( ShortByteString)
import qualified Data.ByteString.Short as BS (toShort, fromShort)
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Swagger
import           Control.Lens
import           Servant.API
import           Data.Char (isAlphaNum)
import           Crypto.Hash.SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString as BS
import           Data.Word (Word8)
import qualified Data.Text.Encoding as TE
import           Control.Monad (replicateM)
import           System.Random

import           Database.Persist
import           Database.Persist.Sql

newtype Hash = Hash ShortByteString -- ShortByteString here is just because
               -- regular ByteString is not compatible with Compact regions
  deriving (Show, Generic, Typeable, Eq, Ord)
instance ToJSON Hash where
  toJSON (Hash s) = toJSON $! TE.decodeUtf8 $! BS.fromShort s
instance FromJSON Hash where
  parseJSON = withText "Hash" $ \v-> return $! verifyHash v
instance ToParamSchema Hash where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & format ?~ "b8ab3013e4adb35fae6cbdc9d84c86cd280157b7a93b984c0b40baf7f21b8f72"
instance ToHttpApiData Hash where
  toUrlPiece (Hash t) = toUrlPiece $! TE.decodeUtf8 $! BS.fromShort t
  toQueryParam (Hash t) = toQueryParam $! TE.decodeUtf8 $! BS.fromShort t
instance ToSchema Hash where
  declareNamedSchema _ = pure $ NamedSchema (Just "Hash") $ mempty
    & type_ ?~ SwaggerString
instance FromHttpApiData Hash where
  parseUrlPiece t = Right (verifyHash t)
  parseQueryParam t = Right (verifyHash t)

instance PersistField Hash where
  toPersistValue (Hash s) = toPersistValue $! TE.decodeUtf8 $! BS.fromShort s
  fromPersistValue (PersistText s) = Right $! verifyHash s -- TODO
  fromPersistValue _ = Left $ "InputVerification.hs fromPersistValue Hash , expected Text"
instance PersistFieldSql Hash where
  sqlType _ = SqlString


defaultHash :: Hash
defaultHash = Hash "b8ab3013e4adb35fae6cbdc9d84c86cd280157b7a93b984c0b40baf7f21b8f72"

generateRandomHash :: IO Hash
generateRandomHash = do
  rndBS <- (replicateM 10 $ getStdRandom (randomR (0::Word8, 255::Word8))) >>= return . BS.pack
  let base16 = Base16.encode $! hash rndBS
  return $! Hash $! BS.toShort $! base16

everifyHash:: Text-> Either Text Hash
everifyHash raw =
  case () of
    _ | T.length limitedSize /= 64 -> Left "Hash: wrong size"
    _ | not (T.all isAlphaNum limitedSize ) -> Left "Hash: should be alpha num"
    _ -> Right (Hash $! BS.toShort $! TE.encodeUtf8 limitedSize)
  where
    limitedSize = T.copy $! T.take 64 raw

mverifyHash:: Text-> Maybe Hash
mverifyHash raw =
  case everifyHash raw of
    Left _ -> Nothing
    Right ret -> Just ret

verifyHash:: Text-> Hash
verifyHash raw =
  case everifyHash raw of
    Right ret -> ret
    Left some -> error (show some)
