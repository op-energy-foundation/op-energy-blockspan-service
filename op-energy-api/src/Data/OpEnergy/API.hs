{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Data.OpEnergy.API where

import           Control.Lens
import           Data.Proxy
import           Data.Swagger
import           Servant.API
import           Servant.Swagger

import           Data.OpEnergy.API.V1

backendAPI :: Proxy BackendAPI
backendAPI = Proxy

type BackendAPI =
  "api" :> "v1" :> V1API {- V1 API -}

-- | API for serving @swagger.json@.
type SwaggerAPI = "api" :> "v1" :> "swagger.json" :> Get '[JSON] Swagger

-- | Combined API of a Todo service with Swagger documentation.
type API
  = SwaggerAPI
  :<|> BackendAPI

-- | Swagger spec for Todo API.
apiSwagger :: Swagger
apiSwagger = toSwagger backendAPI
  & info.title   .~ "OpEnergy API"
  & info.version .~ "1.0"
  & info.description ?~ "OpEnergy"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")


