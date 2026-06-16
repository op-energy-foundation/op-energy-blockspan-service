{-# LANGUAGE Safe #-}
{-# LANGUAGE RankNTypes #-}
module OpEnergy.Server.V2.Environment.Logger.Class
  ( Logger(..)
  ) where

import           Data.Text(Text)

data Logger m = Logger
  { logInfo :: Monad m => Text -> m ()

  , logWarning :: Monad m => Text-> m ()

  , logError :: Monad m => Text-> m ()

  , logDebug :: Monad m => Text-> m ()

  , logInput :: Monad m => Text-> m ()

  }

