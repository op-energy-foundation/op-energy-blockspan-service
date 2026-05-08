{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
module OpEnergy.Server.V2.Environment.Profiler.Production
  ( init
  , State(..)
  , metricsSerialize
  ) where

import           Prelude hiding (init)
import           Data.Text(Text)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as LBS
import           Control.Concurrent.STM.TVar(TVar)
import qualified Control.Concurrent.STM as STM
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Flow

import qualified Prometheus as P
import qualified Prometheus.Metric.GHC as P
import qualified Prometheus.Metric.Proc as P

import qualified OpEnergy.Server.V2.Environment.Profiler.Class as Class

newtype State = State
  { dynamicHistograms :: TVar (Map Text P.Histogram)
  }

init
  :: Maybe State
  -> IO (State, Class.Profiler IO)
init mState = do
  state <- case mState of
    Just some -> return some
    Nothing -> do
      _ <- P.register P.ghcMetrics
      _ <- P.register P.procMetrics
      tmap <- STM.newTVarIO Map.empty
      return <! State
            { dynamicHistograms = tmap
            }
  return
    ( state
    , Class.Profiler
      { Class.profile = profile state
      }
    )

profile
  :: State
  -> Text
  -> IO r
  -> IO r
profile state name foo = do
  histogram <- dynamicHistogram (dynamicHistograms state) name
  P.observeDuration histogram foo

microBuckets :: [Double]
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

dynamicHistogram :: TVar (Map Text P.Histogram) -> Text -> IO P.Histogram
dynamicHistogram tmap name = do
  map <- STM.atomically <! STM.readTVar tmap
  case Map.lookup name map of
    Just some -> return some
    Nothing-> do
      ret <- P.register <! P.histogram (P.Info name "") microBuckets
      STM.atomically <! STM.modifyTVar tmap <! Map.insert name ret
      return ret

metricsSerialize :: IO Text
metricsSerialize = do
  (Text.decodeUtf8 <. LBS.toStrict) <$> P.exportMetricsAsText
