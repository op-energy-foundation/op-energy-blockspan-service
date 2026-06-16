{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Environment.BitcoinClient.Class
  ( BitcoinClient(..)
  ) where

import qualified Data.Bitcoin.API as API
import qualified Data.Bitcoin.BlockStats as API
import qualified Data.Bitcoin.BlockInfo as API
import qualified Data.OpEnergy.API.V1.Block as API

import           OpEnergy.Server.V2.Core.Call

data Monad m => BitcoinClient m = BitcoinClient
  { getBlockchainInfo
    :: m (Either Failure API.BlockchainInfo)

  , getBlockStats
    :: API.BlockHeight
    -> m (Either Failure API.BlockStats)

  , getBlock
    :: API.BlockHash
    -> m (Either Failure API.BlockInfo)

  , getBlockHash
    :: API.BlockHeight
    -> m (Either Failure API.BlockHash)

  , getBlockByHash
    :: API.BlockHash
    -> m (Either Failure API.BlockHeader)

  }

