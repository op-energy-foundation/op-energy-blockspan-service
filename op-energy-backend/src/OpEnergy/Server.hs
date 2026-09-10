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
import           Data.Text (Text)
import           Servant ( Application, Proxy(..), ServerT, serve, hoistServer, (:<|>)(..))
import           Network.Wai (Middleware, Response, pathInfo, responseLBS)
import           Network.Wai.Handler.Warp(run)
import           Network.HTTP.Types (status410, hContentType)
import qualified Prometheus as P
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
import qualified OpEnergy.Server.V2 as V2

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

-- | Deprecated v1 path prefixes that have a v2 equivalent. Requests to these
-- paths are disabled (reply with HTTP 410 Gone) when configDisableDeprecatedV1Api
-- is True, pointing the caller at the v2 endpoint. The v1 WebSocket (api/v1/ws)
-- is included because the frontend has migrated to the v2 WebSocket
-- (api/v2/blockspans/ws).
deprecatedV1WithSuccessorPrefixes :: [[Text]]
deprecatedV1WithSuccessorPrefixes =
  [ ["api", "v1", "blockspans", "statistics"]
  , ["api", "v1", "oe", "block"]
  , ["api", "v1", "oe", "blockbyheight"]
  , ["api", "v1", "oe", "blocksbyblockspan"]
  , ["api", "v1", "oe", "blockswithnbdrbyblockspan"]
  , ["api", "v1", "oe", "git-hash"]
  , ["api", "v1", "ws"]
  ]

-- | Deprecated v1 path prefixes that have NO v2 equivalent. They are disabled
-- (reply with HTTP 410 Gone) when configDisableDeprecatedV1Api is True because
-- they are unused by the frontend; since there is no successor, the reply does
-- not point at a v2 endpoint.
deprecatedV1NoSuccessorPrefixes :: [[Text]]
deprecatedV1NoSuccessorPrefixes =
  [ ["api", "v1", "oe", "blockswithhashratebyblockspan"]
  , ["api", "v1", "oe", "blockspanlist"]
  ]

-- | returns API version ("v1"/"v2") of a request path, if the request targets one
apiVersionOfPath :: [Text] -> Maybe Text
apiVersionOfPath (root : version : _)
  | root == "api" && version == "v1" = Just "v1"
  | root == "api" && version == "v2" = Just "v2"
apiVersionOfPath _ = Nothing

-- | True when the request path matches one of the given path prefixes
matchesAnyPrefix :: [[Text]] -> [Text] -> Bool
matchesAnyPrefix prefixes segments =
  any (\prefix -> prefix == take (length prefix) segments) prefixes

-- | returns the HTTP 410 Gone response for a disabled deprecated v1 path, or
-- Nothing when the path is not a disabled v1 endpoint
disabledV1GoneResponse :: [Text] -> Maybe Response
disabledV1GoneResponse segments
  | matchesAnyPrefix deprecatedV1WithSuccessorPrefixes segments =
      Just $ goneResponse "This v1 endpoint is deprecated and has been disabled. Please use the corresponding v2 endpoint."
  | matchesAnyPrefix deprecatedV1NoSuccessorPrefixes segments =
      Just $ goneResponse "This v1 endpoint is deprecated and has been disabled."
  | otherwise = Nothing
  where
    goneResponse msg = responseLBS status410
      [(hContentType, "application/json;charset=utf-8")]
      ("{\"error\":\"" <> msg <> "\"}")

-- | WAI middleware handling the v1 -> v2 migration concerns:
--   1. counts every /api/v1 and /api/v2 request into a prometheus counter,
--      labeled by version, so that v1-vs-v2 traffic can be tracked
--   2. replies with HTTP 410 Gone for deprecated v1 endpoints when they have
--      been disabled via configDisableDeprecatedV1Api
apiMigrationMiddleware :: State -> Middleware
apiMigrationMiddleware s app req respond = do
  let segments = pathInfo req
      Config{ configDisableDeprecatedV1Api = disabled } = config s
      MetricsState{ apiVersionRequests = apiVersionRequests } = metrics s
  case apiVersionOfPath segments of
    Just version -> P.withLabel apiVersionRequests version P.incCounter
    Nothing -> return ()
  case (disabled, disabledV1GoneResponse segments) of
    (True, Just goneResponse) -> respond goneResponse
    _ -> app req respond

-- | Runs HTTP server on a port defined in config in the State datatype
runServer :: (MonadIO m) => AppT m ()
runServer = do
  s <- ask
  let port = configHTTPAPIPort (config s)
  liftIO $ run port (apiMigrationMiddleware s (app s))
  where
    app :: State-> Application
    app s = serve api $ hoistServer api (runAppT s) serverSwaggerBackend
      where
        api :: Proxy API
        api = Proxy
        -- | Combined server of a OpEnergy service with Swagger documentation.
        serverSwaggerBackend :: ServerT API AppM
        serverSwaggerBackend = (return apiSwagger)
          :<|> OpEnergy.Server.V1.websocketHandler
          :<|> ( OpEnergy.Server.V1.server
               :<|> V2.server
               )

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
