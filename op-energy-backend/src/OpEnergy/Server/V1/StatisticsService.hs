{--| This module defines statistics routine
 -}
module OpEnergy.Server.V1.StatisticsService where

import           Control.Monad (forM)
import           Control.Monad.Reader (ask)
import           Control.Monad.IO.Class(MonadIO)
import qualified Data.List as List
import           Data.Maybe(fromJust)
import           Prometheus(MonadMonitor)
import qualified Prometheus as P

import Data.OpEnergy.API.V1.Block
import Data.OpEnergy.API.V1.Positive
import Data.OpEnergy.API.V1 (NbdrStatistics(..), Statistics(..))
import OpEnergy.Server.V1.Class
import OpEnergy.Server.V1.Metrics
import OpEnergy.Server.V1.Config
import OpEnergy.Server.V1.BlockSpanService
import OpEnergy.Server.V1.BlockHeadersService

-- | This function returns an average and std dev of blocks' discover speed in compare to theoretical discover speed of 10 minites (600 seconds),
-- which shows how fast blocks had been discovered: if it less than 100% - then blocks in block span are being discovered slower than theoretical speed. If it more than 100% - faster.
-- current implementation will try to calculate statistics for 'configStatisticsBlockSpansCount' spans of size 'span' starting from 'startHeight' block
calculateStatistics
  :: BlockHeight
  -- ^ starting block height of a range for which statistics will be generated
  -> Positive Int
  -- ^ size of block span
  -> AppM Statistics
calculateStatistics startHeight span = do
  State{ config = Config { configStatisticsBlockSpansCount = statisticsBlockSpansCount}
       , metrics = MetricsState { calculateStatisticsH = calculateStatisticsH}
       } <- ask
  P.observeDuration calculateStatisticsH $ do
    blockSpans <- getBlockSpanList startHeight span $! positiveFromPositive2 statisticsBlockSpansCount
    discoverSpeeds::[Double] <- forM blockSpans $ \(BlockSpan start end) -> do
      startBlock <- mgetBlockHeaderByHeight start >>= pure . fromJust
      endBlock <- mgetBlockHeaderByHeight end >>= pure . fromJust
      return $! getTheoreticalActualMTPPercents startBlock endBlock
    let avg = (List.foldl' (\acc v -> acc + v) 0.0 discoverSpeeds ) / (fromIntegral statisticsBlockSpansCount)
        stddev = sqrt $! (List.foldl' (\acc i-> acc + (i - avg) ^ (2 :: Int)) 0.0 discoverSpeeds) / (fromIntegral ((unPositive2 statisticsBlockSpansCount) - 1))
    return $ Statistics
      { nbdr = NbdrStatistics
        { avg = avg
        , stddev = stddev
        }
      }

-- | returns ration between actual median time and theoretical median times. Value of 1.0 means, that there is no difference between theoretical and actual median times.
getRealTheoreticalRatio
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockSpan
  -> AppT m Double
getRealTheoreticalRatio (BlockSpan startBlock endBlock) = do
  start <- mgetBlockHeaderByHeight startBlock >>= pure . blockHeaderMediantime . fromJust
  end <- mgetBlockHeaderByHeight endBlock >>= pure . blockHeaderMediantime . fromJust
  let theoreticalBlockCountWithinRange = (fromIntegral (end - start )) / 600.0
  return (theoreticalBlockCountWithinRange / (fromIntegral ( endBlock - startBlock)))

-- this function return ratio between theoretical mediantime end-start difference and actual mediantime end-start
getTheoreticalActualMTPPercents
  :: BlockHeader
    -- ^ start block height
  -> BlockHeader
    -- ^ end block height
  -> Double
getTheoreticalActualMTPPercents startBlock endBlock = theoreticalMTP / realMTP
  where
    !theoreticalMTP = fromIntegral ((blockHeaderHeight endBlock - blockHeaderHeight startBlock ) * 600 * 100)
    !realMTP = fromIntegral (blockHeaderMediantime endBlock - blockHeaderMediantime startBlock)
