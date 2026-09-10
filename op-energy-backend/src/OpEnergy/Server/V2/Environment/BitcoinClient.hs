{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Environment.BitcoinClient
  ( getBlockchainInfo
  , getBlockStats
  , getBlock
  , getBlockByHash
  , getBlockHash
  ) where

import           Control.Monad.Reader( asks)
import           Control.Monad.Trans( lift)
import           Flow

import qualified Data.Bitcoin.API as API
import qualified Data.Bitcoin.BlockStats as API
import qualified Data.Bitcoin.BlockInfo as API
import qualified Data.OpEnergy.API.V1.Block as API

import           OpEnergy.Server.V2.Core.App
import           OpEnergy.Server.V2.Core.Call
import           OpEnergy.Server.V2.Environment.Profiler
import qualified OpEnergy.Server.V2.Environment as Env
import qualified OpEnergy.Server.V2.Environment.BitcoinClient.Class
                 as BitcoinClient

getBlockchainInfo
  :: Monad m
  => AppM transactionROM transactionM m (Either Failure API.BlockchainInfo)
getBlockchainInfo =
    let name = "getBlockchainInfo"
    in profile name <| do
  ptr <- asks Env.bitcoinClient
  lift <| BitcoinClient.getBlockchainInfo ptr

getBlockStats
  :: Monad m
  => API.BlockHeight
  -> AppM transactionROM transactionM m (Either Failure API.BlockStats)
getBlockStats args =
    let name = "getBlockStats"
    in profile name <| do
  ptr <- asks Env.bitcoinClient
  lift <| BitcoinClient.getBlockStats ptr args

getBlock
  :: Monad m
  => API.BlockHash
  -> AppM transactionROM transactionM m (Either Failure API.BlockInfo)
getBlock args =
    let name = "getBlock"
    in profile name <| do
  ptr <- asks Env.bitcoinClient
  lift <| BitcoinClient.getBlock ptr args

getBlockHash
  :: Monad m
  => API.BlockHeight
  -> AppM transactionROM transactionM m (Either Failure API.BlockHash)
getBlockHash args =
    let name = "getBlockHash"
    in profile name <| do
  ptr <- asks Env.bitcoinClient
  lift <| BitcoinClient.getBlockHash ptr args

getBlockByHash
  :: Monad m
  => API.BlockHash
  -> AppM transactionROM transactionM m (Either Failure API.BlockHeader)
getBlockByHash args =
    let name = "getBlockByHash"
    in profile name <| do
  ptr <- asks Env.bitcoinClient
  lift <| BitcoinClient.getBlockByHash ptr args
