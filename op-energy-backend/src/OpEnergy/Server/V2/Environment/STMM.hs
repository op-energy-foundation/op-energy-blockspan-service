{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Environment.STMM
  ( atomically
  ) where

import           Control.Monad.Reader(asks)
import           Control.Monad.Trans(lift)
import           Flow
import qualified Control.Concurrent.STM as STM

import           OpEnergy.Server.V2.Core.App(AppM)
import qualified OpEnergy.Server.V2.Environment as Env
import qualified OpEnergy.Server.V2.Environment.STMM.Class as Class

atomically
  :: Monad m
  => STM.STM r
  -> AppM transactionROM transactionM m r
atomically foo = do
  ptr <- asks Env.stm
  lift <| Class.atomically ptr foo

