{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Environment.Profiler.Manual
  ( init
  , State(..)
  ) where

import           Prelude hiding (init)
import           Data.Text(Text)
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
  -> IO (State, Class.Profiler IO)
init logAction = do
  let
      state = State
        {
        }
  return
    ( state
    , Class.Profiler
      { Class.profile = \callstack foo -> do
        logAction (Env.Profiler <| Request.Profile <| Cast <| Just callstack)
        profile state callstack foo
      }
    )

profile
  :: State
  -> Text
  -> IO r
  -> IO r
profile _ _ foo = foo


