{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Environment.Time.Manual
  ( init
  , shiftTimeNS
  , setTimeNS
  , State(..)
  ) where

import           Prelude hiding (init)
import           Data.Time.Clock(UTCTime)
import           Data.Time.Clock.POSIX
                   ( POSIXTime
                   , posixSecondsToUTCTime
                   )
import           Data.Word(Word64)
import           Control.Monad.Trans.Except( runExceptT, ExceptT(..))
import           Control.Concurrent.STM.TVar(TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Concurrent.STM as STM
import           Flow

import qualified OpEnergy.Server.V2.Environment.Time.Class as Class
import qualified OpEnergy.Server.V2.Environment.Time.Request as Request
import qualified OpEnergy.Server.V2.Environment.Request as Env
import           OpEnergy.Server.V2.Core.Call

newtype State = State
  { nanosecondsV :: TVar Word64
  }

init
  :: ( Env.Request-> IO ())
  -> IO (State, Class.TimeM IO)
init logAction = do
  nsV <- TVar.newTVarIO 0
  let
      state = State
        { nanosecondsV = nsV
        }
  return
    ( state
    , Class.TimeM
      { Class.getCurrentTime = logAction1
        (Env.Time . Request.GetCurrentTime . Call0 . Just)
        (getCurrentTime state)

      , Class.getPOSIXTime = logAction1
        (Env.Time . Request.GetPOSIXTime . Call0 . Just)
        (getPOSIXTime state)

      }
    )
  where
  logAction1 :: (r-> Env.Request)-> IO r-> IO r
  logAction1 toRequest foo = do
    v <- foo
    logAction (toRequest v)
    return v

getCurrentTime
  :: State
  -> IO (Either Failure UTCTime)
getCurrentTime state = runExceptT <! do
  posixTime <- ExceptT <! getPOSIXTime state
  return <! posixSecondsToUTCTime posixTime

getPOSIXTime
  :: State
  -> IO (Either Failure POSIXTime)
getPOSIXTime state = do
   (ret :: Word64) <- STM.atomically <! TVar.readTVar (nanosecondsV state)
   return <! Right <! fromIntegral <! ret `div` 1_000_000_000

shiftTimeNS
  :: State
  -> Word64
  -> IO ()
shiftTimeNS state shiftValue = do
  STM.atomically <! TVar.modifyTVar (nanosecondsV state) (+shiftValue)

setTimeNS
  :: State
  -> Word64
  -> IO ()
setTimeNS state newTime = do
  STM.atomically <! TVar.writeTVar (nanosecondsV state) newTime



