{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Environment.Time.Request
  ( Request(..)
  ) where

import           Data.Time.Clock(UTCTime)
import           Data.Time.Clock.POSIX(POSIXTime)
import           OpEnergy.Server.V2.Core.Call


data Request
  = GetCurrentTime (Call0 UTCTime)
  | GetPOSIXTime (Call0 POSIXTime)
  deriving (Show)

--
--  { getCurrentTime :: m UTCTime
--
--  , getPOSIXTime :: m POSIXTime
--
--  , getTime :: Clock.Clock-> m Clock.TimeSpec
--
--  , readIORef :: Monad m => forall r. IORef r-> m r
--
--  , threadDelay :: Monad m => Int-> m ()
--
--  , yield :: Monad m => m ()
--
--  , async :: Monad m => forall r. m r -> m (Async r)
--
--  , asyncBound :: Monad m => forall r. m r -> m (Async r)
--
--  , waitAnyCancel :: Monad m => forall r. [ Async r] -> m (Async r, r)
--
--  , waitAny :: Monad m => forall r. [ Async r] -> m (Async r, r)
--
--  , wait :: Monad m => forall r. Async r -> m r
--
--  , cancel :: Monad m => forall r. Async r -> m ()
--
--  , poll :: Monad m => forall r. Async r -> m (Maybe (Either SomeException r))
--
--  , lookupEnv :: Text -> m (Either Text (Maybe Text))
--
--  , generateRandom :: (Monad m) => forall r. Random.UniformRange r => (r, r) -> m r
--
--  , atomically :: Monad m => forall r. STM.STM r -> m r
--
--  , generate :: Monad m => forall r. QC.Gen r-> m r
--
--  , encryptIO :: Monad m => ClientSession.Key -> ByteString-> m ( Either Text ByteString)
--
--  , forkIO :: Monad m => m () -> m ThreadId
--
--  , forkIOCatch :: Monad m => (SomeException-> m ()) -> m () -> m () -> m ThreadId
--
--  , generateRandomUUID :: Monad m => m UUID
--
--  , runIO :: Monad m => IO () -> m ()
--  }


