{-- |
 - this module's goal is to be entrypoint between all the backend versions. Currently, there is onty V1 version.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
module OpEnergy.Server where

import           System.IO as IO
import           Servant ( Application, Proxy(..), ServerT, serve, hoistServer, (:<|>)(..))
import           Network.Wai.Handler.Warp(run)
import           Control.Monad.Trans.Reader (ask)
import           Control.Concurrent (threadDelay)
import           Control.Monad.IO.Class(liftIO, MonadIO)
import           Control.Monad.IO.Unlift(MonadUnliftIO)
import           Control.Monad.Logger (MonadLoggerIO, askLoggerIO, logDebug, LoggingT, NoLoggingT, runLoggingT, filterLogger)
import qualified Control.Concurrent.MVar as MVar
import           Control.Concurrent.Async

import           Prometheus(MonadMonitor)

import           Data.OpEnergy.API
import           Data.OpEnergy.API.V1.Positive
import           OpEnergy.Server.V1
import           OpEnergy.Server.V1.Config
import           OpEnergy.Server.V1.Class (AppT, AppM, State(..), defaultState, runAppT, runLogging)
import           OpEnergy.Server.V1.BlockHeadersService (loadDBState, syncBlockHeaders)
import           OpEnergy.Server.V1.DB
import           OpEnergy.Server.V1.Metrics

-- required by prometheus-client
instance MonadMonitor (LoggingT IO)
instance MonadMonitor (NoLoggingT IO)

-- | reads config from file and opens DB connection
initState
  :: ( MonadLoggerIO m
     , MonadUnliftIO m
     )
  => Config
  -> m (State, Async ())
initState config = do
  logFunc <- askLoggerIO
  let Config{ configLogLevelMin = logLevelMin} = config
      filterUnwantedLevels _source level = level >= logLevelMin
      runLogging action = runLoggingT (filterLogger filterUnwantedLevels action) logFunc
  pool <- runLogging $ OpEnergy.Server.V1.DB.getConnection config
  metricsV <- liftIO $ MVar.newEmptyMVar -- prometheus's thread will put value into this variable
  prometheusA <- liftIO $ asyncBound $ OpEnergy.Server.V1.Metrics.runMetricsServer config metricsV
  metrics <- liftIO $ MVar.readMVar metricsV
  state <- defaultState config metrics logFunc pool
  return (state, prometheusA)

-- | Runs HTTP server on a port defined in config in the State datatype
runServer :: (MonadIO m) => AppT m ()
runServer = do
  s <- ask
  let port = configHTTPAPIPort (config s)
  liftIO $ run port (app s)
  where
    app :: State-> Application
    app s = serve api $ hoistServer api (runAppT s) serverSwaggerBackend
      where
        api :: Proxy API
        api = Proxy
        -- | Combined server of a OpEnergy service with Swagger documentation.
        serverSwaggerBackend :: ServerT API AppM
        serverSwaggerBackend = (return apiSwagger)
          :<|> OpEnergy.Server.V1.server

-- | tasks, that should be running during start
bootstrapTasks :: (MonadLoggerIO m, MonadMonitor m) => State -> m ()
bootstrapTasks s = runAppT s $ do
  OpEnergy.Server.V1.BlockHeadersService.loadDBState -- first, load DB state
  OpEnergy.Server.V1.BlockHeadersService.syncBlockHeaders -- check for missing blocks

-- | main loop of the scheduler. Exception in this procedure will cause app to fail
schedulerMainLoop :: (MonadIO m, MonadMonitor m) => AppT m ()
schedulerMainLoop = do
  State{ config = Config{ configSchedulerPollRateSecs = delaySecs }} <- ask
  runLogging $ $(logDebug) "scheduler main loop"
  liftIO $ IO.hFlush stdout
  OpEnergy.Server.V1.schedulerIteration
  liftIO $ threadDelay ((fromPositive delaySecs) * 1000000)
  schedulerMainLoop
