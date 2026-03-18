{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module OpEnergy.Server.V2.Environment.DataSource.Request
  ( Request(..)
  ) where

import           Data.OpEnergy.API.V1.Block
                   ( BlockHeight
                   , BlockHeader
                   )
import           OpEnergy.Server.V2.Core.Call


data Request
  = forall r. WithTransactionRO (Call0 r)
  | forall r. WithTransaction (Call0 r)
  | StoreBlockHeader (Call BlockHeader ())
  | MGetBlockHeaderByHeightRO (Call BlockHeight (Maybe BlockHeader))




