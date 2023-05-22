{-- | This module provides block headers cache based on 2-dimensional vector, such that lookup and insert time is expected to be O(1)
 -}
{-# LANGUAGE TemplateHaskell     #-}
module OpEnergy.Server.V1.BlockHeadersService.Vector.Cache
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
import           Data.Vector(Vector)
import           Data.Vector.Mutable(IOVector)
import qualified Data.Vector.Mutable as VM
import           GHC.Compact (Compact)
import qualified GHC.Compact as Compact

import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Positive
import           OpEnergy.Server.V1.Config

data BlockHeadersCaches = BlockHeadersCaches
  { immutableHeightCache :: IOVector (Compact(Vector BlockHeader))
    -- ^ this is immutable part of cache that keeps all the blocks older than tip - configCacheChunkSize
  , mutableHeightCache :: IOVector BlockHeader
    -- ^ this is mutable part of cache that keeps newer than tip - configCacheChunkSize blocks. As soon as it will be filled, it will move to immutable cache
  , hashCache :: Compact( Map BlockHash BlockHeight)
    -- ^ immutable cache of Hash -> Height map. This is needed as we support /api/v1/oe/block call, which requires hash -> block association
  , cacheTop :: Maybe BlockHeight
  -- ^ this is the pointer to cache top. It is expected, that cache is only filled monotonically from the bottom (index 0), such that there should be no holes
  }

-- | Wrapper for a type. MVar is used here as it is difficult to update Cache in
-- STM as such update requires IO action (mutable vectors? compact regions, etc).
-- It is expected, that cache write will use takeMVar/putMVar. Cache read requires only readMVar
newtype BlockHeadersCache = BlockHeadersCache (MVar BlockHeadersCaches)

init
  :: MonadIO m
  => Config
  -> m BlockHeadersCache
init (Config{ configCacheChunkSize = configCacheChunkSize})= do
  mutableCache <- liftIO $ VM.new (fromPositive configCacheChunkSize)
  immutableCache <- liftIO $ VM.new 0
  hashCache <- liftIO $ Compact.compact Map.empty
  v <- liftIO $ MVar.newMVar (BlockHeadersCaches { immutableHeightCache = immutableCache
                                                 , mutableHeightCache = mutableCache
                                                 , hashCache = hashCache
                                                 , cacheTop = Nothing
                                                 } )
  return (BlockHeadersCache v)
