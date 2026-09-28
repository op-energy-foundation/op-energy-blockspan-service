{-# LANGUAGE Safe #-}
{-# LANGUAGE RankNTypes #-}
module OpEnergy.Server.V2.Environment.DataSource.Class
  ( DataSource(..)
  ) where

import           Data.OpEnergy.API.V1.Block
                   ( BlockHeight
                   , BlockHash
                   , BlockHeader
                   )
import           OpEnergy.Server.V2.Core.Call(Failure)

data DataSource transactionROM transactionM m = DataSource
  { withTransactionRO
    :: forall r
    . Show r
    => transactionROM r
    -> m (Either Failure r)

  , withTransaction
    :: forall r
    . Show r
    => transactionM r
    -> m (Either Failure r)

  , storeBlockHeader
    :: Monad transactionM
    => BlockHeader
    -> transactionM ()

  , mgetBlockHeaderByHeightRO
    :: Monad transactionROM
    => BlockHeight
    -> transactionROM (Maybe BlockHeader)

  , mgetBlockHeaderByHashRO
    :: Monad transactionROM
    => BlockHash
    -> transactionROM (Maybe BlockHeader)

  }



