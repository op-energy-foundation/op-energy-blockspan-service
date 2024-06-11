{-# LANGUAGE OverloadedStrings #-}
module Data.OpEnergy.API.V1.Error
  ( jsonError
  )where

import           Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson

import           Servant(ServerError, errBody, errHeaders)

-- | The goal of this function is to wrap error's description into JSON format
-- which is mostly used for API responses
-- usage example: jsonError err400 ("user not found"::Text)
jsonError :: ToJSON a => ServerError-> a-> ServerError
jsonError err reason = err { errBody = Aeson.encode reason, errHeaders = [ ("Content-type", "application/json") ]}
