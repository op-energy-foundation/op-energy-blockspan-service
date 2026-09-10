{-# LANGUAGE Safe #-}
{-# LANGUAGE RankNTypes #-}
module OpEnergy.Server.V2.Environment.DataSource
  ( withTransactionRO
  , withTransaction
  ) where

import           Control.Monad.Trans( lift)
import           Control.Monad.Reader( asks)
import           Flow

import qualified OpEnergy.Server.V2.Environment.DataSource.Class as Class
import qualified OpEnergy.Server.V2.Environment as Env
import           OpEnergy.Server.V2.Core.Call(Failure)
import           OpEnergy.Server.V2.Core.App(AppM)

withTransactionRO
  :: ( Monad m
     , Monad transactionROM
     , Show r
     )
  => transactionROM r
  -> AppM transactionROM transactionM m (Either Failure r)
withTransactionRO foo = do
  ptr <- asks Env.dataSource
  lift <| Class.withTransactionRO ptr foo

withTransaction
  :: ( Monad m
     , Monad transactionM
     , Show r
     )
  => transactionM r
  -> AppM transactionROM transactionM m (Either Failure r)
withTransaction foo = do
  ptr <- asks Env.dataSource
  lift <| Class.withTransaction ptr foo




