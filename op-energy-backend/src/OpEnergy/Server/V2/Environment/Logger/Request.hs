{-# LANGUAGE Safe #-}
{-# LANGUAGE RankNTypes #-}
module OpEnergy.Server.V2.Environment.Logger.Request
  ( Request(..)
  ) where

import           Data.Text(Text)

import           OpEnergy.Server.V2.Core.Call

data Request
  = LogInfo (Cast Text)

  | LogWarning (Cast Text)

  | LogError (Cast Text)

  | LogDebug (Cast Text)

  | LogInput (Cast Text)


