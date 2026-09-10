{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Environment.Profiler.Dummy
  ( init
  , State(..)
  ) where

import           Prelude hiding (init)
import           Flow

import qualified OpEnergy.Server.V2.Environment.Profiler.Class as Class

data State = State
  {
  }

init
  :: Maybe State
  -> IO (State, Class.Profiler IO)
init mState = do
  state <- case mState of
    Just some -> return some
    Nothing -> do
      return <! State
            {
            }
  return
    ( state
    , Class.Profiler
      { Class.profile = \_ foo -> foo
      }
    )

