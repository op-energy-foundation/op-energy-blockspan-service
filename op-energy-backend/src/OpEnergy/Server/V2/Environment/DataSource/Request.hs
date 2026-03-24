{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module OpEnergy.Server.V2.Environment.DataSource.Request
  ( Request(..)
  , ReadRequest(..)
  , WriteRequest(..)
  ) where

import           Data.OpEnergy.API.V1.Block
                   ( BlockHeight
                   , BlockHeader
                   )
import           OpEnergy.Server.V2.Core.Call

data ReadRequest
  = MGetBlockHeaderByHeightRO (Call BlockHeight (Maybe BlockHeader))

data WriteRequest
  = StoreBlockHeader (Call BlockHeader ())

data Request
  = forall r. WithTransactionRO (Call0 r)
  | forall r. WithTransaction (Call0 r)
  | WriteRequest WriteRequest
  | ReadRequest ReadRequest




