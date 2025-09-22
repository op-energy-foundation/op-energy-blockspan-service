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
import qualified Data.HashSet.InsOrd as InsOrd
import qualified Data.Text as T
import           Servant.API
import           Servant.Swagger
import           Servant.API.WebSocket (WebSocket)

import           Data.OpEnergy.API.V1
import           Data.OpEnergy.API.V2

type BackendAPI =
  ( "api" :> "v1" :> V1API {- V1 API -} )
  :<|> ( "api" :> "v2" :> "blockspans" :> V2API {- V2 API -} )

backendAPI :: Proxy BackendAPI
backendAPI = Proxy

websocketBackendAPI :: Proxy WebSocketBackendAPI
websocketBackendAPI = Proxy

-- | API for serving @swagger.json@.
type SwaggerAPI = "api" :> "v1" :> "swagger.json" :> Get '[JSON] Swagger

-- | API for WebSocket connection. It have to be separate as Websocket does not support servant-client so it should be apart from BackendAPI
type WebSocketAPI = "api" :> "v1" :> "ws" :> WebSocket
  :<|> "api" :> "v2" :> "blockspans" :> "ws" :> WebSocket

-- | This type describes both websocket and backend APIs
type WebSocketBackendAPI = WebSocketAPI :<|> BackendAPI

-- | Combined API of a Todo service with Swagger documentation.
type API
  = SwaggerAPI
  :<|> WebSocketBackendAPI

-- | Swagger spec for Todo API.
apiSwagger :: Swagger
apiSwagger = toSwagger websocketBackendAPI
  & info.title   .~ "OpEnergy Blockspans API"
  & info.version .~ "1.0"
  & info.description ?~ "OpEnergy blockspans service API"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")
  & paths.itraversed %@~ addBlockSpanTagsToV2Paths

addBlockSpanTagsToV2Paths :: String -> PathItem -> PathItem
addBlockSpanTagsToV2Paths pathKey pathItem = 
  if "/api/v2/blockspans/" `T.isInfixOf` T.pack pathKey
  then addTagToPathItem pathItem
  else pathItem
  where
    addTagToPathItem :: PathItem -> PathItem
    addTagToPathItem item = item 
      { _pathItemGet = addTagToOperation <$> _pathItemGet item
      , _pathItemPost = addTagToOperation <$> _pathItemPost item
      , _pathItemPut = addTagToOperation <$> _pathItemPut item
      , _pathItemDelete = addTagToOperation <$> _pathItemDelete item
      }
    
    addTagToOperation :: Operation -> Operation
    addTagToOperation op = op { _operationTags = InsOrd.singleton "BlockSpan" }


