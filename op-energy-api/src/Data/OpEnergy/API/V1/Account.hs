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
module Data.OpEnergy.API.V1.Account where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Text                  (Text)

newtype AccountToken = AccountToken Text
  deriving (Show, Generic, Typeable)
instance ToJSON AccountToken
instance FromJSON AccountToken
instance ToSchema AccountToken where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "AccountToken schema"
    & mapped.schema.example ?~ toJSON defaultAccountToken
instance ToParamSchema AccountToken where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & format ?~ "b8ab3013e4ddb35fae6cedc9d84c86fd280157b7a93b984c0b40baf7f21b8f72"

defaultAccountToken :: AccountToken
defaultAccountToken = AccountToken "a86c139a32e7dac42afe4265a955a0fd9d8c2885e26c7e92d4270b3813faa356"

newtype AccountSecret = AccountSecret Text
  deriving (Show, Generic, Typeable)
instance ToJSON AccountSecret
instance FromJSON AccountSecret
instance ToSchema AccountSecret where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "AccountSecret schema"
    & mapped.schema.example ?~ toJSON defaultAccountSecret
instance ToParamSchema AccountSecret where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & format ?~ "b8ab3013e4ddb35fae6cedc9d84c86fd280157b7a93b984c0b40baf7f21b8f72"


defaultAccountSecret :: AccountSecret
defaultAccountSecret = AccountSecret "a86c139a32e7dac42afe4265a955a0fd9d8c2885e26c7e92d4270b3813faa356"

