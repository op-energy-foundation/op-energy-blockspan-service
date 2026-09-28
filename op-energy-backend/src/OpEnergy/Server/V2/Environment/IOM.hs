{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Environment.IOM
  ( threadDelay
  ) where

import           Control.Monad.Reader(asks)
import           Control.Monad.Trans(lift)

import           OpEnergy.Server.V2.Core.App
import qualified OpEnergy.Server.V2.Environment.IOM.Class as Class
import qualified OpEnergy.Server.V2.Environment as Env

threadDelay
  :: Monad m
  => Int
  -> AppM transactionROM transaction m ()
threadDelay delay = do
  ptr <- asks Env.io
  lift $ Class.threadDelay ptr delay


