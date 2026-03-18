{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Environment.IOM.Class
  ( IOM(..)
  ) where

import qualified System.Random.Stateful as Random

data Monad m => IOM m = IOM
  { threadDelay :: Monad m => Int-> m ()

  , generateRandom
    :: (Monad m)
    => forall r.(Show r, Random.UniformRange r)
    => (r, r)
    -> m r
  }



