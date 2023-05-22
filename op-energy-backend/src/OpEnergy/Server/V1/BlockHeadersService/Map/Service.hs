{-- | This module provides block headers cache based on 2-dimensional vector, such that lookup time is expected to be O(1)
 -}
{-# LANGUAGE TemplateHaskell     #-}
module OpEnergy.Server.V1.BlockHeadersService.Map.Service
  ( insertEnsureCapacity
  , maybeInsert
  , maybeInsertMany
  , lookupByHeight
  , lookupByHash
  , ensureCapacity
  ) where

import           Data.IORef(newIORef, readIORef, writeIORef)
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map.Strict as Map
import           Control.Monad(when, foldM)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Exception(bracket)
import qualified GHC.Compact as Compact

import           Data.OpEnergy.API.V1.Block
import           OpEnergy.Server.V1.Class ( AppT, State(..))
import           OpEnergy.Server.V1.Metrics hiding (ensureCapacity)
import           OpEnergy.Server.V1.BlockHeadersService.Map.Cache

import           Prometheus(MonadMonitor)
import qualified Prometheus as P

-- | This function tries to lookup BlockHeader from cache by given block height. It is assuming, that cache is add-only and boxed. Thus, write operation is actually writes pointer to a block header, so 
lookupByHeight
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockHeight
  -> AppT m (Maybe BlockHeader)
lookupByHeight height = do
  State{ blockHeadersCache = BlockHeadersCache cacheV
       , metrics = MetricsState { mgetBlockHeaderByHeightCacheH = mgetBlockHeaderByHeightCacheH
                                , mgetBlockHeaderByHeightCacheHit = mgetBlockHeaderByHeightCacheHit
                                , mgetBlockHeaderByHeightCacheMiss = mgetBlockHeaderByHeightCacheMiss
                                }
       } <- ask
  P.observeDuration mgetBlockHeaderByHeightCacheH $ do
    BlockHeadersCaches {heightCache = heightCacheC, cacheTop = mcacheTop} <- liftIO $ MVar.readMVar cacheV
    case mcacheTop of
      Nothing -> do  -- nothing had been cached yet
        P.incCounter mgetBlockHeaderByHeightCacheMiss
        return Nothing
      Just cacheTop
        | height > cacheTop -> do -- haven't witnessed such height yet
            P.incCounter mgetBlockHeaderByHeightCacheMiss
            return Nothing
        | otherwise -> do
          P.incCounter mgetBlockHeaderByHeightCacheHit
          return $! Map.lookup height $! Compact.getCompact heightCacheC

-- | This function tries to lookup BlockHeader from cache by given block height. It is assuming, that cache is add-only and boxed. Thus, write operation is actually writes pointer to a block header, so 
lookupByHash
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockHash
  -> AppT m (Maybe BlockHeader)
lookupByHash hash = do
  State{ blockHeadersCache = BlockHeadersCache cacheV
       , metrics = MetricsState { mgetBlockHeaderByHashCacheH = mgetBlockHeaderByHashCacheH
                                , mgetBlockHeaderByHashCacheHit = mgetBlockHeaderByHashCacheHit
                                , mgetBlockHeaderByHashCacheMiss = mgetBlockHeaderByHashCacheMiss
                                }
       } <- ask
  mheight <- P.observeDuration mgetBlockHeaderByHashCacheH $ do
    BlockHeadersCaches { hashCache = hashCache}  <- liftIO $ MVar.readMVar cacheV
    case Map.lookup hash $! Compact.getCompact hashCache of
      Nothing -> do
        P.incCounter mgetBlockHeaderByHashCacheMiss
        return Nothing
      Just height -> do
        P.incCounter mgetBlockHeaderByHashCacheHit
        return (Just height)
  case mheight of
    Nothing -> return Nothing
    Just height -> lookupByHeight height

-- | try to insert header into block header cache. Will not attempt to grow cache if there is not enough space and will throw error instead.
maybeInsert
  :: MonadIO m
  => BlockHeader
  -> AppT m ()
maybeInsert !header = do
  State{ blockHeadersCache = BlockHeadersCache cacheV
       , metrics = MetricsState { mgetBlockHeaderByHeightCacheInsert = mgetBlockHeaderByHeightCacheInsert
                                , mgetBlockHeaderByHashCacheInsert = mgetBlockHeaderByHashCacheInsert
                                }
       } <- ask
  failedV <- liftIO $ newIORef True
  liftIO $ bracket
    (MVar.takeMVar cacheV) -- read cache state and set default failed state to True, which should be changed to False on success
    (\v -> readIORef failedV >>= \failed-> when failed (MVar.putMVar cacheV v)) -- if failed - put back an old state
    $ \cache@(BlockHeadersCaches { heightCache = heightCache, cacheTop = mcacheTop, hashCache = hashCache}) -> do
      case mcacheTop of
        Nothing | blockHeaderHeight header > 0 -> error "you are trying to cache block header not from the 0 block. This will create a hole and will cacuse exceptions in the future! Cache from block 0"
        Just cacheTop
          | blockHeaderHeight header < cacheTop -> return () -- do nothing, given block header should already be cached
          | blockHeaderHeight header - cacheTop > 1 -> error "cached block headers' heights should be monotonically increasing, otherwise there will be undefined behavior"
        _ -> do -- error cases checked, should be safe to continue
          !newHeightCache <-P.observeDuration mgetBlockHeaderByHeightCacheInsert $! do
            let !ret = Map.insert (blockHeaderHeight header) header $! Compact.getCompact heightCache
            liftIO $! Compact.compact ret
          !newHashCache <- P.observeDuration mgetBlockHeaderByHashCacheInsert $ do
            let !ret = Map.insert (blockHeaderHash header) (blockHeaderHeight header) $ Compact.getCompact hashCache
            liftIO $! Compact.compact ret
          MVar.putMVar cacheV (cache { heightCache = newHeightCache, cacheTop = Just (blockHeaderHeight header), hashCache = newHashCache})
          writeIORef failedV False
          return ()

ensureCapacity
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockHeight
  -> AppT m ()
ensureCapacity _ = return ()
  
-- | ensureCapacity and then maybeInsert
insertEnsureCapacity :: (MonadIO m, MonadMonitor m) => BlockHeader-> AppT m ()
insertEnsureCapacity header = do
  ensureCapacity (blockHeaderHeight header)
  maybeInsert header

maybeInsertMany
  :: MonadIO m
  => [BlockHeader]
  -> AppT m ()
maybeInsertMany headers = do
  State{ blockHeadersCache = BlockHeadersCache cacheV
       , metrics = MetricsState { mgetBlockHeaderByHeightCacheInsert = mgetBlockHeaderByHeightCacheInsert
                                , mgetBlockHeaderByHashCacheInsert = mgetBlockHeaderByHashCacheInsert
                                }
       } <- ask
  failedV <- liftIO $ newIORef True
  liftIO $ bracket
    (MVar.takeMVar cacheV) -- read cache state and set default failed state to True, which should be changed to False on success
    (\v -> readIORef failedV >>= \failed-> when failed (MVar.putMVar cacheV v)) -- if failed - put back an old state
    $ \cache@(BlockHeadersCaches { heightCache = heightCache, cacheTop = mcacheTop, hashCache = hashCache}) -> do
      let forEach (heightCache, hashCache, mcacheTop) header = do 
            case mcacheTop of
              Nothing | blockHeaderHeight header > 0 -> error "you are trying to cache block header not from the 0 block. This will create a hole and will cacuse exceptions in the future! Cache from block 0"
              Just cacheTop
                | blockHeaderHeight header < cacheTop -> return (heightCache, hashCache, mcacheTop) -- do nothing, given block header should already be cached
                | blockHeaderHeight header - cacheTop > 1 -> error "cached block headers' heights should be monotonically increasing, otherwise there will be undefined behavior"
              _ -> do -- error cases checked, should be safe to continue
                !newHeightCache <-P.observeDuration mgetBlockHeaderByHeightCacheInsert $! do
                  let !ret = Map.insert (blockHeaderHeight header) header heightCache
                  return ret
                !newHashCache <- P.observeDuration mgetBlockHeaderByHashCacheInsert $ do
                  let !ret = Map.insert (blockHeaderHash header) (blockHeaderHeight header) hashCache
                  return ret
                return (newHeightCache, newHashCache, Just (blockHeaderHeight header))
      (newHeightCache, newHashCache, newCacheTop) <- foldM forEach (Compact.getCompact heightCache, Compact.getCompact hashCache, mcacheTop) headers
      newHeightCacheC <- Compact.compact newHeightCache
      newHashCacheC <- Compact.compact newHashCache
      MVar.putMVar cacheV (cache { heightCache = newHeightCacheC, cacheTop = newCacheTop, hashCache = newHashCacheC})
      writeIORef failedV False
      return ()
