{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Environment.Profiler.Request
  ( Request(..)
  ) where

import           Data.Text(Text)
import           OpEnergy.Server.V2.Core.Call


data Request
  = Profile (Cast Text)
  deriving (Show)

