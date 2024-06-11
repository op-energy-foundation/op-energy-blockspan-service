{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.OpEnergy.API.V1.Error
  ( jsonError
  , throwJSON
  )where

import           Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import           Control.Monad.Error.Class(MonadError)

import           Servant(ServerError, errBody, errHeaders, throwError)

-- | The goal of this function is to wrap error's description into JSON format
-- which is mostly used for API responses
-- usage example: jsonError err400 ("user not found"::Text)
jsonError :: ToJSON a => ServerError-> a-> ServerError
jsonError err reason = err { errBody = Aeson.encode reason, errHeaders = [ ("Content-type", "application/json") ]}

-- | this is the replacement of `throwError err400` with `jsonError err400 "something went wrong"`
throwJSON :: (Monad m, MonadError ServerError m, ToJSON a) => ServerError-> a -> m b
throwJSON err v = throwError $ jsonError err v
