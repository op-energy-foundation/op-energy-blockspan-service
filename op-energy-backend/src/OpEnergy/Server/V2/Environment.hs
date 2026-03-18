{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Environment
  ( Environment(..)
  ) where

import           Data.Text(Text)
import           Control.Concurrent.STM.TVar (TVar)

import           Data.OpEnergy.API.V1.Block (BlockHeader)

import qualified OpEnergy.Server.V1.Config as Config
import qualified OpEnergy.Server.V2.Environment.DataSource.Class as DataSource
import qualified OpEnergy.Server.V2.Environment.Logger.Class as Logger
import qualified OpEnergy.Server.V2.Environment.Profiler.Class as Profiler
import qualified OpEnergy.Server.V2.Environment.IOM.Class as IOM
import qualified OpEnergy.Server.V2.Environment.STMM.Class as STMM
import qualified OpEnergy.Server.V2.Environment.Time.Class as Time
import qualified OpEnergy.Server.V2.Environment.BitcoinClient.Class as BitcoinClient

data Environment transactionROM transactionM m = Environment
  { callstack :: Text

  , mcurrentTipV :: TVar (Maybe BlockHeader)

  , shutdownRequestedV :: TVar Bool

  , config :: Config.Config

  , dataSource :: DataSource.DataSource transactionROM transactionM m

  , logger :: Logger.Logger m

  , profiler :: Profiler.Profiler m

  , io :: IOM.IOM m

  , stm :: STMM.STMM m

  , time :: Time.TimeM m

  , bitcoinClient :: BitcoinClient.BitcoinClient m

  }
