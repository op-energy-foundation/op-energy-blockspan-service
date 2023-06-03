{--
 - This module defines data type that keep all the state, used by backend
 -}
module OpEnergy.Server.V1.Class where

import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.Trans.Reader (runReaderT, ReaderT, ask)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans(lift)
import           Control.Monad.Logger (runLoggingT, filterLogger, LoggingT, MonadLoggerIO, Loc, LogSource, LogLevel, LogStr)
import qualified Data.Map as Map
import           Servant (Handler)
import           Data.Pool(Pool)
import           Database.Persist.Postgresql (SqlBackend)
import           System.IO(hFlush, stdout)

import           Data.OpEnergy.API.V1.Block ( BlockHeader)
import           OpEnergy.Server.V1.Config
import           OpEnergy.Server.V1.Metrics
import           OpEnergy.Server.V1.BlockHeadersService.Vector.Cache as Cache

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

-- | defines the whole state used by backend
data State = State
  { config :: Config
  -- ^ app config, loaded from file
  , blockHeadersDBPool :: Pool SqlBackend
  -- ^ DB connection pool to BlockHeadersDB
  , blockHeadersCache :: BlockHeadersCache
  -- ^ BlockHeaders' cache
  , currentTip :: TVar (Maybe BlockHeader)
  -- ^ defines the newest witnessed confirmed block
  , logFunc :: LogFunc
  , logLevel :: TVar LogLevel
  , metrics :: MetricsState
  -- ^ contains metrics handlers
  }

type AppT = ReaderT State
type AppM = ReaderT State Handler

-- | constructs default state with given config and DB pool
defaultState :: (MonadLoggerIO m ) => Config-> MetricsState-> LogFunc-> Pool SqlBackend-> m State
defaultState config metrics logFunc _blockHeadersDBPool = do
  _blockHeadersCache <- Cache.init config
  _blockHeadersHashCache <- liftIO $ TVar.newTVarIO Map.empty
  _currentTip <- liftIO $ TVar.newTVarIO Nothing
  logLevelV <- liftIO $ TVar.newTVarIO (configLogLevelMin config)
  return $ State
    { config = config
    , blockHeadersCache = _blockHeadersCache
    , blockHeadersDBPool = _blockHeadersDBPool
    , currentTip = _currentTip -- websockets' init data relies on whole BlockHeader
    , logFunc = logFunc
    , logLevel = logLevelV
    , metrics = metrics
    }

-- | Runs app transformer with given context
runAppT :: (Monad m) => State-> AppT m a-> m a
runAppT s x = runReaderT x s

runLogging :: MonadIO m => LoggingT m a -> AppT m ()
runLogging loggingAction = do
  State{ logFunc = logFunc, config = Config{ configLogLevelMin = logLevelMin}} <- ask
  let filterUnwantedLevels _source level = level >= logLevelMin
  _ <- lift $ runLoggingT (filterLogger filterUnwantedLevels loggingAction) logFunc
  liftIO $ hFlush stdout
  return ()
