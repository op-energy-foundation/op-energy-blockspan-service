{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Environment.IOM.Test
  ( init
  , State(..)
  ) where

import           Prelude hiding (init)
import           Flow
import qualified Control.Concurrent as IO

import qualified System.Random.Stateful as Random

import           OpEnergy.Server.V2.Core.Call
import qualified OpEnergy.Server.V2.Environment.Request as Env
import qualified OpEnergy.Server.V2.Environment.IOM.Class as Class
import qualified OpEnergy.Server.V2.Environment.IOM.Request as Request

data State = State
  {
  }

init
  :: ( Env.Request-> IO ())
  -> IO (State, Class.IOM IO)
init logAction = do
  let
      state = State
        {
        }
  return
    ( state
    , Class.IOM
      { Class.threadDelay = \delay -> do
        logAction <| Env.IOM <| Request.ThreadDelay <| Cast <| Just delay
        IO.threadDelay 1000 -- well, we won't want it to delay for a long time
                            -- during test

      , Class.generateRandom = \arg -> do
        ret <- generateRandom arg
        logAction <| Env.IOM <| Request.GenerateRandom
          <| Call  (Just arg) (Just (Right ret))
        return ret
      }
    )

generateRandom :: Random.UniformRange r => (r, r) -> IO r
generateRandom r = do
  Random.applyAtomicGen (Random.uniformR r) Random.globalStdGen


