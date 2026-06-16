module OpEnergy.Server.V2.Environment.Test
  ( State(..)
  , init
  , initConfig
  , initConfigProfilerProdState
  ) where

import           Prelude hiding (init)
import           Data.Text(Text)
import           Control.Concurrent.STM(STM)
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import           Data.Sequence(Seq(..))
import qualified Data.Sequence as Seq
import           Data.Vector(Vector)
import           Flow

import qualified Data.OpEnergy.API.V1.Block as Block

import qualified OpEnergy.Server.V2.Environment as Env
import qualified OpEnergy.Server.V2.Environment.Time.Manual as Time
import qualified OpEnergy.Server.V2.Environment.Request as EnvRequest
import qualified OpEnergy.Server.V2.Environment.Profiler.Manual as Profiler
import qualified OpEnergy.Server.V2.Environment.Profiler.Dummy as ProdProfiler
import qualified OpEnergy.Server.V2.Environment.Logger.Test as Logger
import qualified OpEnergy.Server.V2.Environment.BitcoinClient.Test as BitcoinClient
import qualified OpEnergy.Server.V2.Environment.DataSource.Test as DataSource
import qualified OpEnergy.Server.V2.Environment.STMM.Production as STMM
import qualified OpEnergy.Server.V2.Environment.IOM.Test as IOM
import qualified OpEnergy.Server.V1.Config as Config

data State = State
  { timeState :: Time.State
  , requestLogV :: TVar (Seq EnvRequest.Request)
  , bitcoinClientState :: BitcoinClient.State
  , dataSourceState :: DataSource.State
  , prodProfilerState :: ProdProfiler.State
  }

initConfigProfilerProdState
  :: Text
  -> (Config.Config-> Config.Config)
  -> Maybe ProdProfiler.State
  -> Vector Block.BlockHeader
  -> IO (State, Env.Environment STM STM IO)
initConfigProfilerProdState context configUpdate mProdState blockchain = do
  logV::(TVar (Seq EnvRequest.Request)) <- TVar.newTVarIO Seq.empty
  shutdownRequestedV <- TVar.newTVarIO False
  mCurrentConfirmedTipV <- TVar.newTVarIO Nothing
  mWitnessedUnconfirmedTipV <- TVar.newTVarIO Nothing
  let
      logRequestSTM :: EnvRequest.Request-> STM ()
      logRequestSTM !req = do
        TVar.modifyTVar logV <! \reqLog ->
          let
              !newLog = reqLog :|> req
          in newLog
      logRequest :: EnvRequest.Request-> IO ()
      logRequest !req = do
        STM.atomically <! do
          logRequestSTM req
  (timeStateInstance, timeSrv) <- Time.init logRequest
  (prodProfilerState, prodProfiler) <- ProdProfiler.init mProdState
  (_, profilerInstance) <- Profiler.init logRequest prodProfiler
  (_, loggerInstance) <- Logger.init logRequest
  (dataSourceState, dataSourceInstance) <- DataSource.init logRequest
    logRequestSTM
  stm <- STMM.init
  (bitcoinClientInstance, bitcoinClient) <- BitcoinClient.init context blockchain
    timeSrv logRequest profilerInstance
  (_, io ) <- IOM.init logRequest
  let
      state = State
        { timeState = timeStateInstance
        , requestLogV = logV
        , bitcoinClientState = bitcoinClientInstance
        , dataSourceState = dataSourceState
        , prodProfilerState = prodProfilerState
        }
  return
    ( state
    , Env.Environment
      { Env.config = configUpdate Config.defaultConfig
      , Env.callstack = context
      , Env.shutdownRequestedV = shutdownRequestedV
      , Env.mCurrentConfirmedTipV = mCurrentConfirmedTipV
      , Env.mWitnessedUnconfirmedTipV = mWitnessedUnconfirmedTipV

      , Env.io = io

      , Env.dataSource = dataSourceInstance
      , Env.bitcoinClient = bitcoinClient
      , Env.stm = stm
      , Env.logger = loggerInstance
      , Env.profiler = profilerInstance
      , Env.time = timeSrv
      }
    )

init
  :: Text
  -> Vector Block.BlockHeader
  -> IO (State, Env.Environment STM STM IO)
init context = initConfig context id



initConfig
  :: Text
  -> (Config.Config-> Config.Config)
  -> Vector Block.BlockHeader
  -> IO (State, Env.Environment STM STM IO)
initConfig context configUpdate blockchain =
  initConfigProfilerProdState context configUpdate Nothing blockchain
