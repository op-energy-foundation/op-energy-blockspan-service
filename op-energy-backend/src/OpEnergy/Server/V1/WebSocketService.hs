{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module OpEnergy.Server.V1.WebSocketService where

import           Data.Aeson as Aeson
import           Data.Text(Text)
import           Data.Text.Show (tshow)
import           Control.Exception as E
import           Control.Monad ( forever)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.Logger (logDebug, logError)
import           Data.IORef
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import           Network.WebSockets ( Connection, receiveData, withPingThread, sendTextData)

import           OpEnergy.Server.V1.Class (runLogging, AppT, State(..), runAppT)
import           Data.OpEnergy.API.V1.Hash( Hash, generateRandomHash)
import           Data.OpEnergy.API.V1.Block( BlockHeight, BlockHeader(..))
import           Data.OpEnergy.API.V1.Positive(naturalFromPositive)
import           OpEnergy.Server.V1.WebSocketService.Message
import           OpEnergy.Server.V1.Config



-- | This procedure is an mainloop for every websocket connection, which:
-- - handles requests from clients;
-- - sends notification about newest confirmed block
-- - sends keepalive packets
webSocketConnection :: MonadIO m => Connection-> AppT m ()
webSocketConnection conn = do
  state <- ask
  liftIO $ bracket (runAppT state $ initConnection state) (\uuid -> runAppT state $ closeConnection state uuid) $ \(uuid, witnessedHeightV)-> do
    let State{ config = Config { configWebsocketKeepAliveSecs = configWebsocketKeepAliveSecs} } = state
    timeoutCounterV <- newIORef (naturalFromPositive configWebsocketKeepAliveSecs)
    liftIO $ withPingThread conn 1 (checkIteration state uuid witnessedHeightV timeoutCounterV) $ forever $ runAppT state $ do
      req <- liftIO $ receiveData conn
      case req of
        ActionWant topics -> sendTopics topics -- handle requested topics
        ActionPing -> do
          liftIO $ do
            sendTextData conn MessagePong
            writeIORef timeoutCounterV (naturalFromPositive configWebsocketKeepAliveSecs)
        ActionInit -> do
          runLogging $ $(logDebug) (tshow uuid <> " init data request")
          mpi <- getMempoolInfo
          liftIO $ do
            sendTextData conn $ Aeson.encode mpi
            writeIORef timeoutCounterV (naturalFromPositive configWebsocketKeepAliveSecs )
  where
    checkIteration state _ witnessedHeightV timeoutCounterV = do
      let State{ currentTip = currentTipV, config = Config {configWebsocketKeepAliveSecs = configWebsocketKeepAliveSecs} } = state
      mwitnessedHeight <- readIORef witnessedHeightV
      mcurrentTip <- STM.atomically $ TVar.readTVar currentTipV
      case (mwitnessedHeight, mcurrentTip) of
        ( _, Nothing) -> do -- haven't witnessed current tip and there is no tip loaded yet. decrease timeout
          decreaseTimeoutOrSendPing state
        (Nothing, Just currentTip) -> do -- current connection saw no current tip yet: need to send one to the client and store
          sendTextData conn $ MessageNewestBlockHeader currentTip
          writeIORef timeoutCounterV (naturalFromPositive configWebsocketKeepAliveSecs)
          writeIORef witnessedHeightV $! Just $! blockHeaderHeight currentTip
        ( Just witnessedHeight, Just currentTip)
          | witnessedHeight == blockHeaderHeight currentTip -> decreaseTimeoutOrSendPing state -- current tip had already been witnessed
          | otherwise -> do -- notify client about newest confirmed block
              sendTextData conn $ MessageNewestBlockHeader currentTip
              writeIORef timeoutCounterV (naturalFromPositive configWebsocketKeepAliveSecs)
              writeIORef witnessedHeightV $! Just $! blockHeaderHeight currentTip
      where
        decreaseTimeoutOrSendPing :: State-> IO ()
        decreaseTimeoutOrSendPing state = do
          let State{ config = Config { configWebsocketKeepAliveSecs = configWebsocketKeepAliveSecs} } = state
          counter <- readIORef timeoutCounterV
          if counter == 0
            then do
              writeIORef timeoutCounterV (naturalFromPositive configWebsocketKeepAliveSecs)
              sendTextData conn ("{\"pong\": true}" :: Text)
            else writeIORef timeoutCounterV (pred counter) -- decrease counter

    initConnection :: MonadIO m => State-> AppT m (Hash, IORef (Maybe BlockHeight))
    initConnection _state = do
      witnessedTipV <- liftIO $ newIORef Nothing
      uuid <- liftIO $ generateRandomHash
      runLogging $ $(logDebug) (tshow uuid <> ": new websocket connection")
      return (uuid, witnessedTipV)

    closeConnection _ (uuid, _) = do
      runLogging $ $(logDebug) (tshow uuid <> ": closed websocket connection")
      return ()

    sendTopics :: MonadIO m => [Text] -> AppT m ()
    sendTopics [] = return ()
    sendTopics ("generatedaccounttoken" : rest) = do -- for now send dummy secret/token
      liftIO $ sendTextData conn ("{\"generatedAccountSecret\" : \"test\", \"generatedAccountToken\": \"test\"}"::Text)
      sendTopics rest
    sendTopics ( topic : rest) = do
      runLogging $ $(logError) ("received unsupported ActionWant " <> topic)
      sendTopics rest

-- currently, frontend expects 'mempool info' initial message from backend. This function provides such info
getMempoolInfo :: AppT IO MempoolInfo
getMempoolInfo = do
  State{ currentTip = currentTipV} <- ask
  mtip <- liftIO $ TVar.readTVarIO currentTipV
  return $ MempoolInfo
    { newestConfirmedBlock = mtip
    }
