{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Environment.Request
  ( Request(..)
  ) where

import qualified OpEnergy.Server.V2.Environment.IOM.Request as IOM
import qualified OpEnergy.Server.V2.Environment.Time.Request as Time
import qualified OpEnergy.Server.V2.Environment.Profiler.Request as Profiler
import qualified OpEnergy.Server.V2.Environment.Logger.Request as Logger
import qualified OpEnergy.Server.V2.Environment.BitcoinClient.Request
                 as BitcoinClient
import qualified OpEnergy.Server.V2.Environment.DataSource.Request as DataSource

data Request
  = IOM IOM.Request
  | Time Time.Request
  | Profiler Profiler.Request
  | Logger Logger.Request
  | BitcoinClient BitcoinClient.Request
  | DataSource DataSource.Request
  deriving (Show)


