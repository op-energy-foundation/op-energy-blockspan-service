{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Environment.IOM.Request
  ( Request(..)
  ) where

import           OpEnergy.Server.V2.Core.Call

data Request
  = ThreadDelay (Cast Int)

  | forall r. (Show r) => GenerateRandom (Call (r,r) r)
instance Show Request where
  show (ThreadDelay some) = "Thread " ++ show some
  show (GenerateRandom some) = "GenerateRandom " ++ show some
