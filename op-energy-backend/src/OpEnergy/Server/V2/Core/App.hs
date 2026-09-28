{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
module OpEnergy.Server.V2.Core.App
  ( AppM(..)
  , runAppM
  ) where

import           Data.Text(Text)

import           Control.Monad.Reader(MonadReader(..))
import           Control.Monad.Trans.Reader(ReaderT(..))
import           Control.Monad.Trans(MonadTrans(..))

import           OpEnergy.Server.V2.Environment(Environment(..))

newtype AppM transactionROM transactionM m a = AppM
  { unAppM :: (ReaderT (Environment transactionROM transactionM m) m) a
  }
  deriving (Functor, Applicative, Monad)
instance Monad m => MonadReader (Environment transactionROM transactionM m) (AppM transactionROM transactionM m) where
  ask = AppM ask
  local f (AppM m) = AppM (local f m)
instance MonadTrans (AppM transactionROM transactionM) where
  lift = AppM . lift

runAppM
  :: Monad m
  => Text
  -> Environment transactionROM transactionM m
  -> AppM transactionROM transactionM m r
  -> m r
runAppM callstack env next = runReaderT
  (unAppM next)
  (env{ callstack = callstack})




