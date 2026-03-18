module OpEnergy.Server.V2.Environment.Test
  ( State(..)
  , init
  , initConfig
  ) where

import           Prelude hiding (init)
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
  }

initConfig
  :: (Config.Config-> Config.Config)
  -> Vector Block.BlockHeader
  -> IO (State, Env.Environment STM STM IO)
initConfig configUpdate blockchain = do
  logV::(TVar (Seq EnvRequest.Request)) <- TVar.newTVarIO Seq.empty
  shutdownRequestedV <- TVar.newTVarIO False
  mcurrentTipV <- TVar.newTVarIO Nothing
  let
      logRequestSTM :: EnvRequest.Request-> STM ()
      logRequestSTM req = do
        TVar.modifyTVar logV <| \reqLog -> reqLog :|> req
      logRequest :: EnvRequest.Request-> IO ()
      logRequest req = do
        STM.atomically <| do
          logRequestSTM req
  (timeStateInstance, timeSrv) <- Time.init logRequest
  (_, profilerInstance) <- Profiler.init logRequest
  (_, loggerInstance) <- Logger.init logRequest
  (dataSourceState, dataSourceInstance) <- DataSource.init logRequest
    logRequestSTM
  stm <- STMM.init
  (bitcoinClientInstance, bitcoinClient) <- BitcoinClient.init blockchain timeSrv logRequest
  (_, io ) <- IOM.init logRequest
  let
      state = State
        { timeState = timeStateInstance
        , requestLogV = logV
        , bitcoinClientState = bitcoinClientInstance
        , dataSourceState = dataSourceState
        }
  return
    ( state
    , Env.Environment
      { Env.config = configUpdate Config.defaultConfig
      , Env.callstack = "test"
      , Env.shutdownRequestedV = shutdownRequestedV
      , Env.mcurrentTipV = mcurrentTipV

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
  :: Vector Block.BlockHeader
  -> IO (State, Env.Environment STM STM IO)
init = initConfig id



