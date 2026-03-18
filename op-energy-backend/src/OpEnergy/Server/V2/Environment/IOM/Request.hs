{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Environment.IOM.Request
  ( Request(..)
  ) where

import           OpEnergy.Server.V2.Core.Call

data Request
  = ThreadDelay (Cast Int)

  | forall r.GenerateRandom (Call (r,r) r)
