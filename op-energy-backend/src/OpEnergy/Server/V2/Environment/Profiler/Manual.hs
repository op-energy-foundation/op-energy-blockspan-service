{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Environment.Profiler.Manual
  ( init
  , State(..)
  ) where

import           Prelude hiding (init)
import           Flow

import qualified OpEnergy.Server.V2.Environment.Profiler.Class as Class
import qualified OpEnergy.Server.V2.Environment.Profiler.Request as Request
import qualified OpEnergy.Server.V2.Environment.Request as Env
import           OpEnergy.Server.V2.Core.Call

data State = State
  {
  }

init
  :: ( Env.Request-> IO ())
  -> Class.Profiler IO
  -> IO (State, Class.Profiler IO)
init logAction lowLayer = do
  let
      state = State
        {
        }
  return
    ( state
    , Class.Profiler
      { Class.profile = \callstack foo -> do
        logAction (Env.Profiler <| Request.Profile <| Cast <| Just callstack)
        Class.profile lowLayer callstack foo
      }
    )



