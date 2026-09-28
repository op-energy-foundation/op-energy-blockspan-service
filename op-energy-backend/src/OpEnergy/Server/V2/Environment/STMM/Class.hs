{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Environment.STMM.Class
  ( STMM(..)
  , module STM
  ) where

import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM hiding (atomically)

data STMM m = STMM
  { atomically
    :: Monad m
    => forall r
    . STM.STM r
    -> m r
  }

