{-# LANGUAGE Safe #-}
{-# LANGUAGE RankNTypes #-}
module OpEnergy.Server.V2.Environment.Logger
  ( logInput
  , logError
  , logWarning
  , logDebug
  , logInfo
  ) where

import           Data.Text(Text)
import           Control.Monad.Reader( asks)
import           Control.Monad.Trans(lift)

import           OpEnergy.Server.V2.Core.App
import qualified OpEnergy.Server.V2.Environment.Logger.Class as Class
import qualified OpEnergy.Server.V2.Environment as Env

logInput
  :: Monad m
  => Text
  -> AppM transactionROM transactionM m ()
logInput str = do
  ptr <- asks Env.logger
  callstack <- asks Env.callstack
  lift $ Class.logInput ptr (callstack <> ": " <> str)

logInfo
  :: Monad m
  => Text
  -> AppM transactionROM transactionM m ()
logInfo str = do
  ptr <- asks Env.logger
  callstack <- asks Env.callstack
  lift $ Class.logInfo ptr (callstack <> ": " <> str)

logWarning
  :: Monad m
  => Text
  -> AppM transactionROM transactionM m ()
logWarning str = do
  ptr <- asks Env.logger
  callstack <- asks Env.callstack
  lift $ Class.logWarning ptr (callstack <> ": " <> str)

logError
  :: Monad m
  => Text
  -> AppM transactionROM transactionM m ()
logError str = do
  ptr <- asks Env.logger
  callstack <- asks Env.callstack
  lift $ Class.logError ptr (callstack <> ": " <> str)

logDebug
  :: Monad m
  => Text
  -> AppM transactionROM transactionM m ()
logDebug str = do
  ptr <- asks Env.logger
  callstack <- asks Env.callstack
  lift $ Class.logDebug ptr (callstack <> ": " <> str)

