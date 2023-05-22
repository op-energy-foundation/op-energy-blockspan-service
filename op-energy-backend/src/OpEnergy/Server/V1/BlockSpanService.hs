{-- | This module provides block spans routines
 --}
module OpEnergy.Server.V1.BlockSpanService where

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Trans.Reader(ask)
import           Prometheus(MonadMonitor)
import qualified Prometheus as P

import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.API.V1.Natural
import           OpEnergy.Server.V1.Class ( AppT, State(..))
import           OpEnergy.Server.V1.Metrics


-- | generates list of block spans starting from given BlockHeight
getBlockSpanList
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockHeight
  -- ^ block span list start
  -> Positive Int
  -- ^ size of spans
  -> Positive Int
  -- ^ number of block spans in resulted list
  -> AppT m [BlockSpan]
getBlockSpanList startHeight span numberOfSpans = do
  State{metrics = MetricsState{ getBlockSpanListH = getBlockSpanListH}} <- ask
  P.observeDuration getBlockSpanListH $ return spans
  where
    _span = fromPositive span
    _start = fromNatural startHeight
    spans = map (\i -> BlockSpan (verifyNatural (_start +  (i * _span))) (verifyNatural (_start + _span + i*_span))) [ 0 .. (fromPositive numberOfSpans) - 1]

-- | returns block span list between start and end blocks, where each span has span size of `span` plus possible reminder span (in case of fractional division on `span`)
getBlockSpanListByRange
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockHeight
    -- ^ starting block height
  -> BlockHeight
    -- ^ end block height
  -> Positive Int
    -- ^ span size
  -> AppT m [BlockSpan]
getBlockSpanListByRange start end span = do
  let (spans, lastSpan) = (end - start) `divMod` naturalFromPositive span
  last <- if lastSpan < 1
        then return [] -- there is no reminder from division
        else getBlockSpanList (start + spans * naturalFromPositive span) (verifyPositiveInt $ fromNatural lastSpan) 1
  main <- getBlockSpanList start span (verifyPositiveInt $ fromNatural spans)
  return (main ++ last)
