{-# LANGUAGE Safe #-}
{-# LANGUAGE RankNTypes #-}
module OpEnergy.Server.V2.Environment.Profiler.Class
  ( Profiler (..)
  ) where

import           Data.Text(Text)

data Profiler m = Profiler
  { profile
    :: forall r
    . Text
    -> m r
    -> m r
  }


