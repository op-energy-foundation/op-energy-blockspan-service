{-- | This module provides block headers cache based on 2-dimensional vector, such
 -- that lookup and insert time is expected to be O(1)
 -}
{-# LANGUAGE TemplateHaskell     #-}
module OpEnergy.Server.V1.BlockHeadersService.Vector.Service
  ( insertEnsureCapacity
  , maybeInsert
  , maybeInsertMany
  , lookupByHeight
  , lookupByHash
  , ensureCapacity
  ) where

import           Data.IORef(newIORef, readIORef, writeIORef)
import qualified Control.Concurrent.MVar as MVar
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Control.Monad(when, foldM)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (logDebug)
import           Data.Vector(Vector, (!))
import qualified Data.Vector as V
import           Data.Vector.Mutable(IOVector)
import qualified Data.Vector.Mutable as VM
import           Control.Exception(bracket)
import           GHC.Compact (Compact)
import qualified GHC.Compact as Compact

import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1.Positive
import           OpEnergy.Server.V1.Config
import           OpEnergy.Server.V1.Class ( runAppT, runLogging, AppT, State(..))
import           OpEnergy.Server.V1.Metrics hiding (blockHeaderHeightCacheEnsureCapacity)
import           OpEnergy.Server.V1.BlockHeadersService.Vector.Cache
import           Data.Text.Show(tshow)

import           Prometheus(MonadMonitor)
import qualified Prometheus as P

-- | This function tries to lookup BlockHeader from cache by given block height.
lookupByHeight
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockHeight
  -> AppT m (Maybe BlockHeader)
lookupByHeight height = do
  State{ config = Config { configCacheChunkSize = configCacheChunkSize }
       , blockHeadersCache = BlockHeadersCache cacheV
       , metrics = MetricsState { blockHeaderHeightCacheH = blockHeaderHeightCacheH
                                , blockHeaderHeightCacheHit = blockHeaderHeightCacheHit
                                , blockHeaderHeightCacheMiss = blockHeaderHeightCacheMiss
                                }
       } <- ask
  P.observeDuration blockHeaderHeightCacheH $ do
    BlockHeadersCaches {immutableHeightCache = immutableHeightCache, mutableHeightCache = mutableHeightCache, cacheTop = mcacheTop} <- liftIO $ MVar.readMVar cacheV
    case mcacheTop of
      Nothing -> do  -- nothing had been cached yet
        P.incCounter blockHeaderHeightCacheMiss
        return Nothing
      Just cacheTop
        | height > cacheTop -> do -- haven't witnessed such height yet
            P.incCounter blockHeaderHeightCacheMiss
            return Nothing
        | otherwise -> do
          let (row,col) = height `divMod` (naturalFromPositive configCacheChunkSize)
              immutableRow = cacheTop `div` (naturalFromPositive configCacheChunkSize)
          if row == immutableRow
            then do -- requested height is currently in the mutable cache
              header <- liftIO $ VM.read mutableHeightCache (fromNatural col)
              P.incCounter blockHeaderHeightCacheHit
              return (Just header)
            else do -- requested height is currently in the immutable cache
              !colCache <- liftIO $ VM.read immutableHeightCache (fromNatural row)
              let !header = (Compact.getCompact colCache) ! (fromNatural col)
              P.incCounter blockHeaderHeightCacheHit
              return (Just header)
              

-- | This function tries to lookup BlockHeader from cache by given block hash
lookupByHash
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockHash
  -> AppT m (Maybe BlockHeader)
lookupByHash hash = do
  State{ blockHeadersCache = BlockHeadersCache cacheV
       , metrics = MetricsState { blockHeaderHashCacheH = blockHeaderHashCacheH
                                , blockHeaderHashCacheHit = blockHeaderHashCacheHit
                                , blockHeaderHashCacheMiss = blockHeaderHashCacheMiss
                                }
       } <- ask
  mheight <- P.observeDuration blockHeaderHashCacheH $ do
    BlockHeadersCaches { hashCache = hashCache}  <- liftIO $ MVar.readMVar cacheV
    case Map.lookup hash (Compact.getCompact hashCache) of
      Nothing -> do
        P.incCounter blockHeaderHashCacheMiss
        return Nothing
      Just height -> do
        P.incCounter blockHeaderHashCacheHit
        return (Just height)
  case mheight of
    Nothing -> return Nothing
    Just height -> lookupByHeight height

-- | try to insert header into block header cache.
maybeInsert
  :: MonadIO m
  => BlockHeader
  -> AppT m ()
maybeInsert !header = maybeInsertMany [header]

-- perform bulk insert into cache
maybeInsertMany
  :: MonadIO m
  => [BlockHeader]
  -> AppT m ()
maybeInsertMany headers = do
  state@State{ config = Config { configCacheChunkSize = configCacheChunkSize }
             , blockHeadersCache = BlockHeadersCache cacheV
             , metrics = MetricsState { blockHeaderHeightCacheInsert = blockHeaderHeightCacheInsert
                                      , blockHeaderHashCacheInsert = blockHeaderHashCacheInsert
                                      }
             } <- ask
  failedV <- liftIO $ newIORef True -- set default failed flag to True, which on success should be changed to False
  liftIO $ bracket
    (MVar.takeMVar cacheV) -- read cache state
    (\v -> readIORef failedV >>= \failed-> when failed (MVar.putMVar cacheV v)) -- if failed - put back an old state
    $ \cache@(BlockHeadersCaches { immutableHeightCache = immutableHeightCache, mutableHeightCache = mutableHeightCache, cacheTop = mcacheTop, hashCache = hashCache}) -> do
      let forEach :: (IOVector (Compact(Vector BlockHeader)), IOVector BlockHeader, Map BlockHash BlockHeight, Maybe BlockHeight) -> BlockHeader -> IO (IOVector (Compact(Vector BlockHeader)), IOVector BlockHeader, Map BlockHash BlockHeight, Maybe BlockHeight)
          forEach (immutableHeightCache, mutableHeightCache, hashCache, mcacheTop) header = do
            let (row, col) = blockHeaderHeight header `divMod` (naturalFromPositive configCacheChunkSize)
            case mcacheTop of
              Nothing
                | blockHeaderHeight header > 0 -> error "you are trying to cache block header not from the 0 block. This will create a hole and will cause exceptions in the future! Cache from block 0"
                | otherwise -> do -- mutable height cache should be already there
                    P.observeDuration blockHeaderHeightCacheInsert $! VM.write mutableHeightCache 0 header
                    !newHashCache <- P.observeDuration blockHeaderHeightCacheInsert $ return $! Map.insert (blockHeaderHash header) (blockHeaderHeight header) hashCache
                    return (immutableHeightCache, mutableHeightCache, newHashCache, Just (blockHeaderHeight header))
              Just cacheTop
                | blockHeaderHeight header <= cacheTop -> return ( immutableHeightCache, mutableHeightCache, hashCache, mcacheTop) -- do nothing, given block header should already be cached
                | blockHeaderHeight header - cacheTop > 1 -> error "cached block headers' heights should be monotonically increasing, otherwise there will be undefined behavior"
                | otherwise -> do -- error cases checked, should be safe to continue
                  let mutableRow = cacheTop `div` (naturalFromPositive configCacheChunkSize)
                  if mutableRow == row
                    then do -- we can just insert given header into currently mutable height cache
                      P.observeDuration blockHeaderHeightCacheInsert $! do
                        VM.write mutableHeightCache (fromNatural col) header
                        return ()
                      !newHashCache <- P.observeDuration blockHeaderHashCacheInsert $ return $! Map.insert (blockHeaderHash header) (blockHeaderHeight header) hashCache
                      return (immutableHeightCache, mutableHeightCache, newHashCache, Just (blockHeaderHeight header))
                    else do -- looks like 'header' should be inserted into the next chunk. We need to store current chunk and create new one
                      runAppT state $ runLogging $ $(logDebug) $! "in order to insert block " <> tshow (blockHeaderHeight header) <> " we need to grow cache chunk: mutableRow" <> tshow mutableRow <> ", row : " <> tshow row <> ", cacheChunkSize: " <> tshow configCacheChunkSize
                      (newImmutableHeightCache,newMutableHeightCache) <- P.observeDuration blockHeaderHeightCacheInsert $! do
                        newRow <- V.freeze mutableHeightCache >>= Compact.compact -- convert to immutable vector and move to out of GC region
                        newImmutableHeightCache <- VM.grow immutableHeightCache 1 -- ensure we have a memory
                        VM.write newImmutableHeightCache (fromNatural mutableRow) newRow -- store previously mutable cache
                        newMutableHeightCache <- VM.new (fromPositive configCacheChunkSize) -- recreate new mutable cache
                        VM.write newMutableHeightCache (fromNatural col) header
                        return (newImmutableHeightCache, newMutableHeightCache)
                      !newHashCache <- P.observeDuration blockHeaderHashCacheInsert $ return $! Map.insert (blockHeaderHash header) (blockHeaderHeight header) hashCache
                      return (newImmutableHeightCache, newMutableHeightCache, newHashCache, Just (blockHeaderHeight header))
      (newImmutableHeightCache, newMutableHeightCache, newHashCache, newCacheTop) <- foldM forEach (immutableHeightCache, mutableHeightCache, Compact.getCompact hashCache, mcacheTop) headers
      newHashCacheC <- Compact.compact newHashCache
      MVar.putMVar cacheV (cache { immutableHeightCache = newImmutableHeightCache, mutableHeightCache = newMutableHeightCache, cacheTop = newCacheTop, hashCache = newHashCacheC})
      writeIORef failedV False -- mark a flag that everything went fine
      return ()

-- | this procedure should ensure, that there is enough capacity to store the
-- upper bound of cache. Currenty, this procedure does nothing as data structure
--implies dynamic resize instead of container pre-allocating 
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
