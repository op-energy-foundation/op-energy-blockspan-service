{-# LANGUAGE Safe #-}
{-# LANGUAGE RankNTypes #-}
module OpEnergy.Server.V2.Environment.Logger.Test
  ( init
  ) where

import           Prelude hiding (init)
import           Flow

import qualified OpEnergy.Server.V2.Environment.Logger.Class as Class
import qualified OpEnergy.Server.V2.Environment.Logger.Request as Request
import qualified OpEnergy.Server.V2.Environment.Request as Env
import           OpEnergy.Server.V2.Core.Call

data State = State
  {
  }

init
  :: ( Env.Request-> IO ())
  -> IO (State, Class.Logger IO)
init logAction = do
  let
      state = State
        {
        }
  return
    ( state
    , Class.Logger
      { Class.logInfo = \str -> do
        logAction (Env.Logger <| Request.LogInfo <| Cast <| Just str)

      , Class.logWarning = \str -> do
        logAction (Env.Logger <| Request.LogWarning <| Cast <| Just str)

      , Class.logError = \str -> do
        logAction (Env.Logger <| Request.LogError <| Cast <| Just str)

      , Class.logDebug = \str -> do
        logAction (Env.Logger <| Request.LogDebug <| Cast <| Just str)

      , Class.logInput = \str -> do
        logAction (Env.Logger <| Request.LogInput <| Cast <| Just str)

      }
    )

