{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Environment.STMM.Production
  ( init
  ) where

import           Prelude hiding (init)
import           Flow

import qualified Control.Concurrent.STM as STM
import qualified OpEnergy.Server.V2.Environment.STMM.Class as Class

init :: IO (Class.STMM IO)
init = do
  return <| Class.STMM
    { Class.atomically = STM.atomically
    }


