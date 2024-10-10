{-# LANGUAGE OverloadedStrings #-}
module OpEnergy.Server.V1.Config where

import           Data.Text (Text)
import           Data.Maybe
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified System.Environment as E
import           Servant.Client (BaseUrl(..), showBaseUrl, parseBaseUrl, Scheme(..))
import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.API.V1.Natural
import           Control.Monad.Catch
import           Control.Monad.Logger(LogLevel(..))

instance MonadThrow Parser where
  throwM = fail . show

instance FromJSON LogLevel where
  parseJSON = withText "LogLevel" $ \v->
    pure $ case v of
      "Debug" -> LevelDebug
      "Info" -> LevelInfo
      "Warn" -> LevelWarn
      "Error" -> LevelError
      other -> LevelOther other

-- | Describes configurable options
data Config = Config
  { configDBPort :: Int
  , configDBHost:: Text
  , configDBUser :: Text
  , configDBName :: Text
  , configDBPassword :: Text
  , configDBConnectionPoolSize :: Positive Int
    -- ^ DB connection pool size
  , configSalt :: Text
    -- ^ this value is being used as a salt for secrets/token generation
  , configHTTPAPIPort :: Int
    -- ^ this port should be used to receive HTTP requests
  , configBTCURL :: BaseUrl
    -- ^ URL to bitcoin node
  , configBTCUser :: Text
  , configBTCPassword :: Text
  , configBTCPollRateSecs :: Positive Int
    -- ^ how often to poll btc node
  , configSchedulerPollRateSecs :: Positive Int
    -- ^ scheduler interval
  , configBlocksToConfirm :: Natural Int
    -- ^ only blocks [ 0 .. (tip - configBlockToConfirm)] are assumed to be confirmed
  , configStatisticsBlockSpansCount :: Positive2 Int
    -- ^ size of a block span to use for calculating statistics
  , configWebsocketKeepAliveSecs :: Positive Int
    -- ^ how many seconds to wait until ping packet will be sent
  , configLogLevelMin :: LogLevel
    -- ^ minimum log level to display
  , configPrometheusPort :: Positive Int
    -- ^ port which should be used by prometheus metrics
  , configCacheChunkSize :: Positive Int
    -- ^ defines size of chunk with which cache is grown
  , configBlockspanMinimumSize :: Positive Int
    -- ^ defines minimum size of blockspan
  , configRecordsPerReply :: Positive Int
    -- ^ defines how many rows shoud be contained within 1 page of the reply/request
  }
  deriving Show
instance FromJSON Config where
  parseJSON = withObject "Config" $ \v-> Config
    <$> ( v .:? "DB_PORT" .!= (configDBPort defaultConfig))
    <*> ( v .:? "DB_HOST" .!= (configDBHost defaultConfig))
    <*> ( v .:? "DB_USER" .!= (configDBUser defaultConfig))
    <*> ( v .:? "DB_NAME" .!= (configDBName defaultConfig))
    <*> ( v .:? "DB_PASSWORD" .!= (configDBPassword defaultConfig))
    <*> ( v .:? "DB_CONNECTION_POOL_SIZE" .!= (configDBConnectionPoolSize defaultConfig))
    <*> ( v .:? "SECRET_SALT" .!= (configSalt defaultConfig))
    <*> ( v .:? "API_HTTP_PORT" .!= (configHTTPAPIPort defaultConfig))
    <*> (( v .:? "BTC_URL" .!= (showBaseUrl $ configBTCURL defaultConfig)) >>= parseBaseUrl)
    <*> ( v .:? "BTC_USER" .!= (configBTCUser defaultConfig))
    <*> ( v .:? "BTC_PASSWORD" .!= (configBTCPassword defaultConfig))
    <*> ( v .:? "BTC_POLL_RATE_SECS" .!= (configBTCPollRateSecs defaultConfig))
    <*> ( v .:? "SCHEDULER_POLL_RATE_SECS" .!= (configSchedulerPollRateSecs defaultConfig))
    <*> ( v .:? "BLOCKS_TO_CONFIRM" .!= (configBlocksToConfirm defaultConfig))
    <*> ( v .:? "STATISTICS_BLOCK_SPANS_COUNT" .!= (configStatisticsBlockSpansCount defaultConfig))
    <*> ( v .:? "WEBSOCKET_KEEP_ALIVE_SECS" .!= (configWebsocketKeepAliveSecs defaultConfig))
    <*> ( v .:? "LOG_LEVEL_MIN" .!= (configLogLevelMin defaultConfig))
    <*> ( v .:? "PROMETHEUS_PORT" .!= (configPrometheusPort defaultConfig))
    <*> ( v .:? "CACHE_CHUNK_SIZE" .!= (configCacheChunkSize defaultConfig))
    <*> ( v .:? "BLOCKSPAN_MINIMUM_SIZE" .!= (configBlockspanMinimumSize defaultConfig))
    <*> ( v .:? "RECORDS_PER_REPLY" .!= (configRecordsPerReply defaultConfig))

defaultConfig:: Config
defaultConfig = Config
  { configDBPort = 5432
  , configDBHost = "localhost"
  , configDBUser = "openergy"
  , configDBName = "openergy"
  , configDBPassword = ""
  , configDBConnectionPoolSize = 32
  , configSalt = ""
  , configHTTPAPIPort = 8999
  , configBTCURL = BaseUrl Http "localhost" 8332 ""
  , configBTCUser = "user"
  , configBTCPassword = "password"
  , configBTCPollRateSecs = verifyPositive 1
  , configSchedulerPollRateSecs = verifyPositive 1
  , configBlocksToConfirm = 6
  , configStatisticsBlockSpansCount = 100
  , configWebsocketKeepAliveSecs = 10
  , configLogLevelMin = LevelWarn
  , configPrometheusPort = 7999
  , configCacheChunkSize = 50000
  , configBlockspanMinimumSize = 6
  , configRecordsPerReply = 1000
  }

getConfigFromEnvironment :: IO Config
getConfigFromEnvironment = do
  configFilePath <- E.lookupEnv "OPENERGY_BACKEND_CONFIG_FILE" >>= pure . fromMaybe "./op-energy-config.json"
  configStr <- BS.readFile configFilePath
  case A.eitherDecodeStrict configStr of
    Left some -> error $ configFilePath ++ " is not a valid config: " ++ some
    Right config -> return config
