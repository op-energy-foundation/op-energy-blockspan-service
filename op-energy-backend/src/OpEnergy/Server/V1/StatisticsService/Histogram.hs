module OpEnergy.Server.V1.StatisticsService.Histogram
  where

import           Prelude hiding (sum, map, min, max)
-- import qualified Prelude as P
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as List

data Histogram = Histogram
  { sum :: Double
  , count :: Int
  , avg :: Double
  , stddev :: Double
  , map :: Map (Double, Double) Int
  , min :: Double
  , max :: Double
  }
  deriving Show

getHistogram
  :: [(Double, Double)]
  -> [Double]
  -> Histogram
getHistogram steps values = Histogram
  { sum = sum
  , count = count
  , avg = average
  , stddev = stddev_
  , map = map
  , min = min
  , max = max
  }
  where
    (_, maxEnd) =
      case steps of
        [] -> (-infinity, -infinity)
        _ -> List.foldl' (\(accstart, accend) (start, end) ->
                            if accend > end
                            then (accstart, accend)
                            else (start, end)
                         ) (head steps) (tail steps)
    initialMap = Map.insert (maxEnd, infinity) 0 $ Map.fromList $! List.map (\v-> (v, 0)) steps
    infinity :: Double = read "Infinity" -- the last bucket's end should be infinity
    (sum, count, map, min, max) = List.foldl' foldMapValue ( 0.0, 0, initialMap, head values, head values) values
    foldMapValue (sum, count, map, accMin, accMax) value = ( sum + value, count + 1, newmap, newMin, newMax)
      where
        newMin = if value < accMin then value else accMin
        newMax = if value > accMax then value else accMax
        newmap = Map.adjust (+1) valueKey map
        keys = Map.keys map
        valueKey = List.foldl' (\(accStart,accEnd) (start,end) ->
                                  if value >= start && value < end
                                  then (start, end)
                                  else (accStart, accEnd)
                               ) (head keys) (tail keys)
    average = sum / (fromIntegral count)
    stddev_ = sqrt ( s / (fromIntegral (count - 1)))
      where
        s = List.foldl' (\acc v-> acc + (v - average) ^ (2::Int)) 0.0 values
