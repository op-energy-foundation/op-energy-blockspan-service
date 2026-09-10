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
                   , BlockHash
                   , BlockHeader
                   )
import           OpEnergy.Server.V2.Core.Call

data ReadRequest
  = MGetBlockHeaderByHeightRO (Call BlockHeight (Maybe BlockHeader))
  | MGetBlockHeaderByHashRO (Call BlockHash (Maybe BlockHeader))
  deriving (Show)

data WriteRequest
  = StoreBlockHeader (Call BlockHeader ())
  deriving (Show)

data Request
  = forall r. (Show r) => WithTransactionRO (Call0 r)
  | forall r. (Show r) => WithTransaction (Call0 r)
  | WriteRequest WriteRequest
  | ReadRequest ReadRequest

instance Show Request where
  show (WriteRequest some) = "WriteRequest " ++ show some
  show (ReadRequest some) = "ReadRequest " ++ show some
  show (WithTransactionRO some) = "WithTransactionRO " ++ show some
  show (WithTransaction some) = "WithTransaction " ++ show some



