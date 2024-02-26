{-- | This module provides block spans routines
 --}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Server.V1.BlockSpanService where

import           Control.Monad.Logger(logError)
import           Control.Monad.Trans.Reader(ask)
import qualified Prometheus as P
import           Servant.Server.Internal.ServerError
import           Servant
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Encoding as Text

import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.API.V1.Natural
import           OpEnergy.Server.V1.Class ( AppM, runLogging, State(..))
import           OpEnergy.Server.V1.Config( Config(..))
import           OpEnergy.Server.V1.Metrics

-- | generates list of block spans starting from given BlockHeight
getBlockSpanList
  :: BlockHeight
  -- ^ block span list start
  -> Positive Int
  -- ^ size of spans
  -> Positive Int
  -- ^ number of block spans in resulted list
  -> AppM [BlockSpan]
getBlockSpanList startHeight span numberOfSpans = do
  State{ metrics = MetricsState{ getBlockSpanListH = getBlockSpanListH}
       , config = Config{ configBlockspanMinimumSize = configBlockspanMinimumSize}
       } <- ask
  P.observeDuration getBlockSpanListH $ do
    if span < configBlockspanMinimumSize
      then do
        let err = "ERROR: requested blockspan size is too small"
        runLogging $ $(logError) err
        throwError err404 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
      else return spans
  where
    _span = fromPositive span
    _start = fromNatural startHeight
    spans = map (\i -> BlockSpan (verifyNatural (_start +  (i * _span))) (verifyNatural (_start + _span + i*_span))) [ 0 .. (fromPositive numberOfSpans) - 1]

-- | returns block span list between start and end blocks, where each span has span size of `span` plus possible reminder span (in case of fractional division on `span`)
getBlockSpanListByRange
  :: BlockHeight
    -- ^ starting block height
  -> BlockHeight
    -- ^ end block height
  -> Positive Int
    -- ^ span size
  -> AppM [BlockSpan]
getBlockSpanListByRange start end span = do
  let spans = (end - start) `div` naturalFromPositive span
  main <- getBlockSpanList start span (verifyPositiveInt $ fromNatural spans)
  return main
