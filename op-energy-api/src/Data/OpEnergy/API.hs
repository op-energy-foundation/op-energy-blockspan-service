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
import           Servant.API.WebSocket (WebSocket)

import           Data.OpEnergy.API.V1

type BackendAPI =
  "api" :> "v1" :> V1API {- V1 API -}

backendAPI :: Proxy BackendAPI
backendAPI = Proxy

websocketBackendAPI :: Proxy WebSocketBackendAPI
websocketBackendAPI = Proxy

-- | API for serving @swagger.json@.
type SwaggerAPI = "api" :> "v1" :> "swagger.json" :> Get '[JSON] Swagger

-- | API for WebSocket connection. It have to be separate as Websocket does not support servant-client so it should be apart from BackendAPI
type WebSocketAPI = "api" :> "v1" :> "ws" :> WebSocket

-- | This type describes both websocket and backend APIs
type WebSocketBackendAPI = WebSocketAPI :<|> BackendAPI

-- | Combined API of a Todo service with Swagger documentation.
type API
  = SwaggerAPI
  :<|> WebSocketBackendAPI

-- | Swagger spec for Todo API.
apiSwagger :: Swagger
apiSwagger = toSwagger websocketBackendAPI
  & info.title   .~ "OpEnergy API"
  & info.version .~ "1.0"
  & info.description ?~ "OpEnergy"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")


