{-- | This module provides block headers cache based on 2-dimensional vector, such that lookup time is expected to be O(1)
 -}
{-# LANGUAGE TemplateHaskell     #-}
module OpEnergy.Server.V1.BlockHeadersService.Map.Cache
  ( BlockHeadersCache(..)
  , BlockHeadersCaches(..)
  , init
  ) where

import           Prelude hiding (init)
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Data.Map.Strict( Map)
import qualified Data.Map.Strict as Map
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           GHC.Compact (Compact)
import qualified GHC.Compact as Compact

import           Data.OpEnergy.API.V1.Block

data BlockHeadersCaches = BlockHeadersCaches
  { heightCache :: Compact(Map BlockHeight BlockHeader)
  , hashCache :: Compact(Map BlockHash BlockHeight)
  , cacheTop :: Maybe BlockHeight
  }

newtype BlockHeadersCache = BlockHeadersCache (MVar BlockHeadersCaches)

init
  :: MonadIO m
  => m BlockHeadersCache
init = do
  heightCache <- liftIO $! Compact.compact Map.empty
  hashCache <- liftIO $! Compact.compact Map.empty
  v <- liftIO $ MVar.newMVar (BlockHeadersCaches { heightCache = heightCache, hashCache = hashCache, cacheTop = Nothing} )
  return (BlockHeadersCache v)
