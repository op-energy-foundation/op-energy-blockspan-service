{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Environment.BitcoinClient.Request
  ( Request(..)
  ) where

import qualified Data.Bitcoin.API as API
import qualified Data.Bitcoin.BlockStats as API
import qualified Data.Bitcoin.BlockInfo as API
import qualified Data.OpEnergy.API.V1.Block as API

import           OpEnergy.Server.V2.Core.Call

data Request = Request
  | GetBlockchainInfo (Call0 API.BlockchainInfo)

  | GetBlockStats (Call API.BlockHeight API.BlockStats)

  | GetBlock (Call API.BlockHash API.BlockInfo)

  | GetBlockHash (Call API.BlockHeight API.BlockHash)




