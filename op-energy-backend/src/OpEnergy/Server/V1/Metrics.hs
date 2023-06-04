{--
 - This module defines data type that keep all the metrics handlers
 -}
module OpEnergy.Server.V1.Metrics where

-- import           System.Clock (Clock(..), diffTimeSpec, getTime, toNanoSecs)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Concurrent.MVar(MVar)
import qualified Control.Concurrent.MVar as MVar
  
import qualified Prometheus as P
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus.Metric.GHC as P
import qualified Prometheus.Metric.Proc as P
import qualified Network.Wai.Handler.Warp as W

import           OpEnergy.Server.V1.Config
import           Data.OpEnergy.API.V1.Positive


-- | defines the whole state used by backend
data MetricsState = MetricsState
  { syncBlockHeadersH :: P.Histogram
  , loadDBStateH :: P.Histogram
  , btcGetBlockchainInfoH :: P.Histogram
  , btcGetBlockHashH :: P.Histogram
  , btcGetBlockH :: P.Histogram
  , btcGetBlockStatsH :: P.Histogram
  , mgetBlockHeaderByHeightH :: P.Histogram
  , mgetBlockHeaderByHashH :: P.Histogram
    -- for mgetBlockHeaderByHeight
  , blockHeaderHeightCacheH :: P.Histogram
  , blockHeaderHeightCacheHit :: P.Counter
  , blockHeaderHeightCacheMiss :: P.Counter
  , blockHeaderHeightCacheInsert :: P.Histogram
  , blockHeaderHeightCacheEnsureCapacity :: P.Histogram
    -- for getBlockHeaderByHash
  , blockHeaderHashCacheH :: P.Histogram
  , blockHeaderHashCacheHit :: P.Counter
  , blockHeaderHashCacheMiss :: P.Counter
  , blockHeaderHashCacheInsert :: P.Histogram
    -- for getBlockSpanList
  , getBlockSpanListH :: P.Histogram
    -- calculateStatistics
  , calculateStatisticsH :: P.Histogram
    -- insertion into DB table BlockHeader
  , blockHeaderDBInsertH :: P.Histogram
  , blockHeaderCacheFromDBLookup :: P.Histogram
  , getBlocksByBlockSpan :: P.Histogram
  , getBlocksWithNbdrByBlockSpan :: P.Histogram
  , getBlocksWithHashrateByBlockSpan :: P.Histogram
  }

-- | constructs default state with given config and DB pool
initMetrics :: MonadIO m => Config-> m MetricsState
initMetrics _config = do
  syncBlockHeadersH <- P.register $ P.histogram (P.Info "syncBlockHeader" "") microBuckets
  loadDBStateH <- P.register $ P.histogram (P.Info "loadDBState" "") microBuckets
  btcGetBlockchainInfoH <- P.register $ P.histogram (P.Info "btcGetBlockchainInfo" "") microBuckets
  btcGetBlockHashH <- P.register $ P.histogram (P.Info "btcGetBlockHashH" "") microBuckets
  btcGetBlockH <- P.register $ P.histogram (P.Info "btcGetBlockH" "") microBuckets
  btcGetBlockStatsH <- P.register $ P.histogram (P.Info "btcGetBlockStatsH" "") microBuckets
  -- mgetBlockHeaderByHeight
  mgetBlockHeaderByHeightH <- P.register $ P.histogram (P.Info "mgetBlockHeaderByHeight" "") microBuckets
  blockHeaderHeightCacheH <- P.register $ P.histogram (P.Info "blockHeaderHeightCache" "") microBuckets
  blockHeaderHeightCacheHit <- P.register $ P.counter (P.Info "blockHeaderHeightCacheHit" "")
  blockHeaderHeightCacheMiss <- P.register $ P.counter (P.Info "blockHeaderHeightCacheMiss" "")
  blockHeaderHeightCacheInsert <- P.register $ P.histogram (P.Info "blockHeaderHeightCacheInsert" "") microBuckets
  -- getBlockHeaderByHash
  mgetBlockHeaderByHashH <- P.register $ P.histogram (P.Info "mgetBlockHeaderByHash" "") microBuckets
  blockHeaderHashCacheH <- P.register $ P.histogram (P.Info "blockHeaderHashCache" "") microBuckets
  blockHeaderHashCacheHit <- P.register $ P.counter (P.Info "blockHeaderHashCacheHit" "")
  blockHeaderHashCacheMiss <- P.register $ P.counter (P.Info "blockHeaderHashCacheMiss" "")
  blockHeaderHashCacheInsert <- P.register $ P.histogram (P.Info "blockHeaderHashCacheInsert" "") microBuckets
  -- getBockSpanList
  getBlockSpanListH <- P.register $ P.histogram (P.Info "getBlockSpanList" "") microBuckets
  calculateStatisticsH <- P.register $ P.histogram (P.Info "calculateStatistics" "") microBuckets
  blockHeaderDBInsertH <- P.register $ P.histogram (P.Info "blockHeaderDBInsert" "") microBuckets
  blockHeaderHeightCacheEnsureCapacity <- P.register $ P.histogram (P.Info "blockHeaderHeightCacheEnsureCapacity" "") microBuckets
  blockHeaderCacheFromDBLookup <- P.register $ P.histogram (P.Info "blockHeaderCacheFromDBLookup" "") microBuckets
  getBlocksByBlockSpan <- P.register $ P.histogram (P.Info "getBlocksByBlockSpan" "") microBuckets
  getBlocksWithNbdrByBlockSpan <- P.register $ P.histogram (P.Info "getBlocksWithNbdrByBlockSpan" "") microBuckets
  getBlocksWithHashrateByBlockSpan <- P.register $ P.histogram (P.Info "getBlocksWithHashrateByBlockSpan" "") microBuckets
  _ <- P.register P.ghcMetrics
  _ <- P.register P.procMetrics
  return $ MetricsState
    { syncBlockHeadersH = syncBlockHeadersH
    , loadDBStateH = loadDBStateH
    , btcGetBlockchainInfoH = btcGetBlockchainInfoH
    , btcGetBlockHashH = btcGetBlockHashH
    , btcGetBlockH = btcGetBlockH
    , btcGetBlockStatsH = btcGetBlockStatsH
    , mgetBlockHeaderByHeightH = mgetBlockHeaderByHeightH
    , mgetBlockHeaderByHashH = mgetBlockHeaderByHashH
    , blockHeaderHeightCacheH = blockHeaderHeightCacheH
    , blockHeaderHeightCacheHit = blockHeaderHeightCacheHit
    , blockHeaderHeightCacheMiss = blockHeaderHeightCacheMiss
    , blockHeaderHeightCacheInsert = blockHeaderHeightCacheInsert
    , blockHeaderHeightCacheEnsureCapacity = blockHeaderHeightCacheEnsureCapacity
    , blockHeaderHashCacheH = blockHeaderHashCacheH
    , blockHeaderHashCacheHit = blockHeaderHashCacheHit
    , blockHeaderHashCacheMiss = blockHeaderHashCacheMiss
    , blockHeaderHashCacheInsert = blockHeaderHashCacheInsert
    , getBlockSpanListH = getBlockSpanListH
    , calculateStatisticsH = calculateStatisticsH
    , blockHeaderDBInsertH = blockHeaderDBInsertH
    , blockHeaderCacheFromDBLookup = blockHeaderCacheFromDBLookup
    , getBlocksByBlockSpan = getBlocksByBlockSpan
    , getBlocksWithNbdrByBlockSpan = getBlocksWithNbdrByBlockSpan
    , getBlocksWithHashrateByBlockSpan = getBlocksWithHashrateByBlockSpan
    }
  where
    microBuckets = [ 0.0000001 -- 100 nanoseconds
                   , 0.00000025 -- 250 ns
                   , 0.0000005 -- 500 ns
                   , 0.000001 -- 1 microsecond
                   , 0.00001 -- 10 microseconds
                   , 0.0001 -- 100 microseconds
                   , 0.00025 -- 250 microseconds
                   , 0.0005 -- 500 microseconds
                   , 0.001 -- 1 ms
                   ] ++ P.defaultBuckets

-- | runs metrics HTTP server
runMetricsServer :: Config -> MVar MetricsState -> IO ()
runMetricsServer config metricsV = do
  let Config{configPrometheusPort = metricsPort } = config
  metrics <- initMetrics config
  MVar.putMVar metricsV metrics
  W.run (fromPositive metricsPort) P.metricsApp
