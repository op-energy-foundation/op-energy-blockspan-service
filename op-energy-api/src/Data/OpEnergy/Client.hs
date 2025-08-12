{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.OpEnergy.Client where

import           Data.Proxy              (Proxy (..))
import           Network.HTTP.Client.TLS as Client
import           Network.HTTP.Client hiding (Proxy)
import           Servant.Client hiding ((//), (/:))
import           Servant.API

import           Data.OpEnergy.API
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.API.V1
import qualified Data.OpEnergy.API.V1 as V1

getStatistics :: BlockHeight-> Positive Int-> ClientM Statistics
getBlock :: BlockHash-> ClientM BlockHeader
getBlockByHeight :: BlockHeight-> ClientM BlockHeader
getBlocksByBlockspan :: BlockHeight-> Positive Int-> Maybe (Positive Int)-> Maybe Bool-> Maybe Bool-> ClientM [BlockSpanHeadersNbdrHashrate]
getBlocksWithNbdrByBlockspan :: BlockHeight-> Positive Int-> Maybe (Positive Int)-> ClientM [BlockSpanHeadersNbdr]
getBlocksWithHashrateByBlockspan :: BlockHeight-> Positive Int-> Maybe (Positive Int)-> ClientM [BlockSpanHeadersHashrate]
getBlockspanlist :: BlockHeight-> Positive Int-> Positive Int-> ClientM [BlockSpan]
getGitHash :: ClientM GitHashResponse

v2getStatistics :: BlockHeight-> Positive Int-> ClientM Statistics
v2getBlock :: BlockHash-> ClientM BlockHeader
v2getBlockByHeight :: BlockHeight-> ClientM BlockHeader
v2getBlocksByBlockspan :: BlockHeight-> Positive Int-> Maybe (Positive Int)-> Maybe Bool-> Maybe Bool-> ClientM [BlockSpanHeadersNbdrHashrate]
v2getBlocksWithNbdrByBlockspan :: BlockHeight-> Positive Int-> Maybe (Positive Int)-> ClientM [BlockSpanHeadersNbdr]
v2getBlocksWithHashrateByBlockspan :: BlockHeight-> Positive Int-> Maybe (Positive Int)-> ClientM [BlockSpanHeadersHashrate]
v2getBlockspanlist :: BlockHeight-> Positive Int-> Positive Int-> ClientM [BlockSpan]
v2getGitHash :: ClientM V1.GitHashResponse

(getStatistics
  :<|> getBlock
  :<|> getBlockByHeight
  :<|> getBlocksByBlockspan
  :<|> getBlocksWithNbdrByBlockspan
  :<|> getBlocksWithHashrateByBlockspan
  :<|> getBlockspanlist
  :<|> getGitHash )

  :<|> ( v2getStatistics
  :<|> v2getBlock
  :<|> v2getBlockByHeight
  :<|> v2getBlocksByBlockspan
  :<|> v2getBlocksWithNbdrByBlockspan
  :<|> v2getBlocksWithHashrateByBlockspan
  :<|> v2getBlockspanlist
  :<|> v2getGitHash )
  = client $ Proxy @BackendAPI

{-
mkClientEnv :: Client.Manager -> BaseUrl -> ClientEnv
mkClientEnv mgr burl = env { makeClientRequest = newMakeClientRequest}
  where
    env = Servant.Client.mkClientEnv mgr burl
    newMakeClientRequest burl req = ((makeClientRequest env) burl req) { Client.queryString = BS.empty}
-}

-- | this function will try to perform API calls to blockspan service
-- within one client environment
-- returns:
--   - Left ClientError - in case of failure
--   - Right result - in case of success
withClientEither :: BaseUrl-> (ClientM a)-> IO (Either ClientError a)
withClientEither url foo = do
  env <- mkClientEnv <$> newManager tlsManagerSettings <*> pure url
  runClientM foo env

-- | this function will try to perform API calls to blockspan service within
-- one client environment.
-- In case of failure will throw an exception with 'error'
withClient :: BaseUrl-> (ClientM a)-> IO a
withClient url foo = do
  eresult <- withClientEither url foo
  case eresult of
    Left some -> error $ "withClient: error: " <> show some
    Right some -> return some

