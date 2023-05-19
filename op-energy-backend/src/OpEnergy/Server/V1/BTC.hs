{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module OpEnergy.Server.V1.BTC where
import           Data.Pool(Pool)
import           OpEnergy.Server.V1.Config
import Database.Persist.Postgresql as DB
import qualified API.Bitcoin as BTC
import qualified Data.List as L
import           OpEnergy.Server.V1.DB
import           Servant.API (BasicAuthData(..))
import           Servant.Client          ( runClientM, parseBaseUrl, BaseUrl(..), Scheme(..))
import           Network.HTTP.Client     (defaultManagerSettings, newManager)
import           Control.Monad (forM_, when)
import           Control.Monad.Reader (lift)
import           Data.Time.Clock
import           Data.OpEnergy.API.V1.InputVerification
import           Data.OpEnergy.API.V1.Hash
import           Servant.Client.JsonRpc
import           Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Scientific
import qualified Data.Map as Map
import           Data.BlockchainInfo.API
import           BlockchainInfo
import           Control.Concurrent
import           System.IO
import           Data.Maybe
import           Data.Conduit (await,yield,leftover,ConduitT,Sink,runConduit, (.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import           Control.Exception (Exception, SomeException, SomeAsyncException)
import qualified Control.Exception as E
import qualified System.IO.Error as E

handle :: (SomeException -> IO a) -> IO a -> IO a
handle hfoo payload = E.handle syncExceptionsHandler (payload >>= E.evaluate)
  where
    syncExceptionsHandler e = do
      let mAsyncException::Maybe SomeAsyncException = E.fromException e
      case mAsyncException of
        Nothing-> hfoo e
        Just some -> do
          print "got SomeAsyncException, rethrowing"
          hFlush stdout
          E.throwIO some


ensureWallet :: Config-> IO BTC.Wallet
ensureWallet config = do
  env <- BTC.mkClientEnv <$> newManager defaultManagerSettings <*> return (configBTCURL config)
  eerr <- flip runClientM env $ do
    ewallets <- BTC.listWallets userPass []
    case ewallets of
      (Result _ wallets ) -> do
        liftIO $ print wallets
        case L.filter (\(BTC.Wallet w) -> w == configBTCWallet config) wallets of
          [] -> do
            eloaded <- BTC.loadWallet userPass [configBTCWallet config]
            liftIO $ print $ "loadWallet = " <> show eloaded
            case eloaded of
              (Result _ (BTC.LoadWalletResponse wallet warning)) | T.length warning < 1 -> do
                liftIO $ print $ "loaded wallet " <> show wallet
                return wallet
              some -> error $ "ensureWallet BTC.loadWallet: " <> show some
          (wallet:_) -> return wallet
      some -> error $ "ensureWallet listWallets: " <> show some
  case eerr of
    Right wallet -> return wallet
    Left some -> do
      liftIO $ print "something went wrong, assume, that wallet is missing, creating new one..."
      ecreated <- flip runClientM env $ BTC.createWallet userPass [configBTCWallet config]
      case ecreated of
        Right (Result _ wallet) -> return $ BTC.name wallet
        some -> error $ "ensureWallet createWallet: " <> show some
  where
    userPass = BasicAuthData (T.encodeUtf8 $ configBTCUser config) (T.encodeUtf8 $ configBTCPassword config)

ensureCurrencyPool :: Config -> Pool SqlBackend-> IO ()
ensureCurrencyPool config pool = do
  wallet <- ensureWallet config
  print "synchronizing state between bitcoin and our DB"
  ensureNoLockWalletMarkedAsFree config pool
  ensureNoMissingWallets config pool wallet
  ensureAvailableWalletsPool config pool wallet

ensureAvailableWalletsPool :: Config-> Pool SqlBackend-> BTC.Wallet-> IO ()
ensureAvailableWalletsPool config pool wallet = do
  availableAddresses <- flip runSqlPersistMPool pool $ count ([ ] :: [Filter BTCWalletFree])
  print $ "availableAddresses " <> show availableAddresses
  let addressesCountToCreate = configWalletsPoolAmount config - availableAddresses
  if addressesCountToCreate <= 0
    then return ()
    else do
      print $ "creating " <> show addressesCountToCreate <> " addresses"
      env <- BTC.mkClientEnv <$> newManager defaultManagerSettings <*> return (configBTCURL config)
      forM_ [1 .. addressesCountToCreate] $ \_ -> do
        eaddress <- flip runClientM env $ BTC.getNewAddress userPass wallet []
        case eaddress of
          Left err -> error $ "ensureCurrencyPool: " <> show err
          Right (Result _ address ) -> do
            now <- getCurrentTime
            flip runSqlPersistMPool pool $ insert $ BTCWalletFree
              { bTCWalletFreeAddress = address
              , bTCWalletFreeCreationTime = now
              }
  where
    userPass = BasicAuthData (T.encodeUtf8 $ configBTCUser config) (T.encodeUtf8 $ configBTCPassword config)

ensureNoMissingWallets :: Config-> Pool SqlBackend-> BTC.Wallet-> IO ()
ensureNoMissingWallets config pool wallet = do
  env <- BTC.mkClientEnv <$> newManager defaultManagerSettings <*> return (configBTCURL config)
  eallAddresses <- flip runClientM env $ BTC.listReceivedByAddress userPass wallet (0, True)
  case eallAddresses of
    Right (Result _ allAddresses) -> do
      print $ "allwallets count " <> show (L.length allAddresses)
      lockedWallets <- flip runSqlPersistMPool pool $ selectList ([ ] :: [Filter BTCWalletLock]) [] -- TODO: stream
      print $ "lockedWallets count " <> show (L.length lockedWallets)
      freeWallets <- flip runSqlPersistMPool pool $ selectList ([ ] :: [Filter BTCWalletFree]) [] -- TODO: stream
      print $ "freeWallets count " <> show (L.length freeWallets)
      let nonLockedAddresses =
            L.foldl' filterLocked allAddresses lockedWallets
            where
              filterLocked :: [BTC.ReceivedByAddress]-> Entity BTCWalletLock-> [BTC.ReceivedByAddress]
              filterLocked allAddresses (Entity _ walletLock) = L.filter (\w -> bTCWalletLockAddress walletLock /= BTC.address w) allAddresses
          missedAddresses =
            L.foldl' filterFree nonLockedAddresses freeWallets
            where
              filterFree :: [BTC.ReceivedByAddress]-> Entity BTCWalletFree-> [BTC.ReceivedByAddress]
              filterFree allAddresses (Entity _ walletFree) = L.filter (\w -> bTCWalletFreeAddress walletFree /= BTC.address w) allAddresses
          missedFreeAddresses = L.filter (\(Entity _ wallet)-> (L.findIndex (\addr -> BTC.address addr == bTCWalletFreeAddress wallet)  allAddresses) == Nothing) freeWallets
          missedLockAddresses = L.filter (\(Entity _ wallet)-> (L.findIndex (\addr -> BTC.address addr == bTCWalletLockAddress wallet)  allAddresses) == Nothing) lockedWallets
      print $ "found " <> show (L.length missedAddresses) <> " missed addresses, that are not locked or free, which should be stored as free"
      print $ "found " <> show (L.length missedFreeAddresses) <> " missed free addresses, which should be deleted"
      print $ "found " <> show (L.length missedLockAddresses) <> " missed locked addresses, which should be handled manually!"
      when (L.length missedAddresses > 0) $ do
        print $ "storing free addresses"
        forM_ missedAddresses $ \address -> do
          now <- getCurrentTime
          flip runSqlPersistMPool pool $ insert $ BTCWalletFree
                { bTCWalletFreeAddress = BTC.address address
                , bTCWalletFreeCreationTime = now
                }
      when (L.length missedFreeAddresses > 0) $ do
        print $ "deleting free addresses"
        forM_ missedFreeAddresses $ \(Entity freeWalletId freeWallet) -> do
          print $ "deleting missed free address " <> show (bTCWalletFreeAddress freeWallet)
          flip runSqlPersistMPool pool $ delete $ freeWalletId
      when (L.length missedLockAddresses > 0) $ do
        print $ "queue notifications for admins with missed locked addresses"
        now <- getCurrentTime
        flip runSqlPersistMPool pool $ insert $ AdminNotification now $ "there are " <> tshow (L.length missedLockAddresses) <> " lock addresses, which are missed from wallet's address list: This is the error case, which should be handled MANUALLY! see /api/v1/missed-lock-addresses for the list of such addresses"
        return ()
      return ()
    some -> error $ "ensureNoMissingWallets: listReceivedByAddress" <> show some
  where
    userPass = BasicAuthData (T.encodeUtf8 $ configBTCUser config) (T.encodeUtf8 $ configBTCPassword config)

ensureNoLockWalletMarkedAsFree :: Config-> Pool SqlBackend-> IO ()
ensureNoLockWalletMarkedAsFree config pool = do
  (lockWallets, freeWallets) <- flip runSqlPersistMPool pool $ (,)
    <$> selectList ([ ] :: [Filter BTCWalletLock]) [] -- TODO: stream
    <*> selectList ([ ] :: [Filter BTCWalletFree]) [] -- TODO: stream
  case L.filter (\(Entity _ freeWallet)-> (L.findIndex (\(Entity _ lockWallet) -> (bTCWalletLockAddress lockWallet) == (bTCWalletFreeAddress freeWallet)) lockWallets) /= Nothing ) freeWallets of
    [] -> return ()
    some -> do
      print $ "there are " <> show (L.length some) <> " locked addresses, that are marked as free. This is error, I will remove free mark"
      flip runSqlPersistMPool pool $ forM_ some $ \(Entity wrongFreeAddressId _)-> delete wrongFreeAddressId
      return ()
  where
    userPass = BasicAuthData (T.encodeUtf8 $ configBTCUser config) (T.encodeUtf8 $ configBTCPassword config)

checkForDeposits:: Config-> Pool SqlBackend-> IO ()
checkForDeposits config pool = do
  lockWallets <- flip runSqlPersistMPool pool $ selectList ([ ] :: [Filter BTCWalletLock]) [ Asc BTCWalletLockLockTime ] -- TODO: stream
  env <- BTC.mkClientEnv <$> newManager defaultManagerSettings <*> return (configBTCURL config)
  forM_ lockWallets $ \(Entity lockWalletId lockWallet) -> do
    ebitcoin <- flip runClientM env $ BTC.getReceivedByAddress userPass wallet ((bTCWalletLockAddress lockWallet), verifyNatural 6)
    case ebitcoin of
      Right (Result _ bitcoin) -> do
        currentUSDCentsPerBTC <- getStoredUSDCentsPerBTC config pool
        let currentSatoshis = BTC.bitcoinToSatoshi bitcoin
            currentUSDCentsDelta = satoshisAsUSDCents currentUSDCentsPerBTC (currentSatoshis - (bTCWalletLockBalanceAtLock lockWallet))
            positiveExpectedDeposit = naturalFromPositive (bTCWalletLockExpectedDeposit lockWallet)
        case () of
          _ | currentUSDCentsDelta >= positiveExpectedDeposit -> do -- got expeceted
            now <- getCurrentTime
            flip runSqlPersistMPool pool $ do
              mperson <- selectFirst [ PersonId ==. bTCWalletLockPersonId lockWallet ] []
              case mperson of
                Nothing -> do
                  insert $ AdminNotification
                    { adminNotificationCreationTime = now
                    , adminNotificationNotification = "Locked wallet " <> (tshow $ bTCWalletLockAddress lockWallet) <> " points to non-existent PersonId: this is not expected and should be handled manually"
                    }
                  return ()
                Just (Entity personId person) -> do
                  insert $ TransactionLog
                    { transactionLogTransactionType = Deposit
                    , transactionLogCurrency = BTC
                    , transactionLogUsdCentsCurrencyRate = currentUSDCentsPerBTC
                    , transactionLogPersonId = bTCWalletLockPersonId lockWallet
                    , transactionLogUsdAmount = positiveFromNatural currentUSDCentsDelta
                    , transactionLogTime = now
                    , transactionLogComment = Nothing
                    , transactionLogTechComment = Nothing
                    }
                  update personId
                    [ PersonDepositSum =. (personDepositSum person) + currentUSDCentsDelta
                    , PersonAvailableBalance =. (personAvailableBalance person) + currentUSDCentsDelta
                    ]
                  insert $ BTCWalletFree
                    { bTCWalletFreeAddress = bTCWalletLockAddress lockWallet
                    , bTCWalletFreeCreationTime = bTCWalletLockCreationTime lockWallet
                    }
                  delete lockWalletId
                  return ()
          _ | currentSatoshis > bTCWalletLockBalanceAtLastCheck lockWallet -> flip runSqlPersistMPool pool $ do -- update AtLastCheck
              update lockWalletId [ BTCWalletLockBalanceAtLastCheck =. currentSatoshis ]
              return ()
          _ -> return () -- nothing to do
      some -> error $ "checkForDeposits getReceivedByAddress: " <> show some
  where
    wallet = BTC.Wallet (configBTCWallet config)
    userPass = BasicAuthData (T.encodeUtf8 $ configBTCUser config) (T.encodeUtf8 $ configBTCPassword config)

satoshisAsUSDCents :: Positive USDCents-> Natural Satoshi-> Natural USDCents
satoshisAsUSDCents (Positive centsPerBTC) (Natural satoshis) = verifyNatural $! floor  $! (fromIntegral centsPerBTC ) * bitcoin
  where
    bitcoin::Double
    bitcoin = (fromIntegral satoshis) / 100000000.0

usdCentsAsSatoshis :: Positive USDCents-> Natural USDCents-> Natural Satoshi
usdCentsAsSatoshis (Positive centsPerBTC) (Natural usdAmount) = verifyNatural $! floor $! usdAmountD / centsPerBTCD
  where
    usdAmountD :: Double
    usdAmountD = fromIntegral (usdAmount * 100000000) -- * 100000000 is for getting results in satoshis
    centsPerBTCD :: Double
    centsPerBTCD = fromIntegral centsPerBTC

getStoredUSDCentsPerBTC :: Config-> Pool SqlBackend-> IO (Positive USDCents)
getStoredUSDCentsPerBTC config pool = do
  mlastUSDCentsPerBTC <- flip runSqlPersistMPool pool $ selectFirst ([ ] :: [Filter BTCUSDCentRate]) [ Desc BTCUSDCentRateCreationTime ]
  case mlastUSDCentsPerBTC of
    Nothing -> error "getCurrentUSDCentsPerBTC: no rate value had been gotten, error"
    Just (Entity recordId record)-> return $ bTCUSDCentRateCentsPerBTC record

getCurrentUSDCentsPerBTC :: Config-> IO (Positive USDCents)
getCurrentUSDCentsPerBTC config = do
  map <- withBlockchainInfo $ tickerAPI
  case Map.lookup "USD" map of
    Nothing-> error "getCurrentUSDCentsPerBTC: USD ticker is missing from reponse"
    Just ticker -> do
      let tickerCents = floor $ (tickerLast ticker) * 100
      return $! verifyPositiveInt $! tickerCents

ensureCurrencyRate :: Config -> Pool SqlBackend-> IO ()
ensureCurrencyRate config pool = do
  print $ "ensuring BTC has USD rate"
  mlastUSDCentsPerBTC <- flip runSqlPersistMPool pool $ selectFirst ([ ] :: [Filter BTCUSDCentRate]) [ Desc BTCUSDCentRateCreationTime ]
  now <- getCurrentTime
  let UTCTime _ nowPicosecs = now
  case mlastUSDCentsPerBTC of
    Just (Entity recordId record)-> do
      let diffSecs = (floor $ nominalDiffTimeToSeconds $! now `diffUTCTime` bTCUSDCentRateCreationTime record)
          UTCTime _ recordPicosecs = bTCUSDCentRateCreationTime record
          recordSinceMidnightSecs = (fromEnum recordPicosecs) `div` 1000000000000
          nowSinceMidnightSecs = (fromEnum nowPicosecs) `div` 1000000000000
      when ( case () of
              _ | nowSinceMidnightSecs < recordSinceMidnightSecs -> True -- we passed midnight and should update rate
              _ | diffSecs >= daySecs -> True -- stored rate is too old
              _ -> False  -- we have fresh enough rate
           ) $ do
        currRate <- getCurrentUSDCentsPerBTC config
        _ <- flip runSqlPersistMPool pool $ insert $ BTCUSDCentRate
          { bTCUSDCentRateCreationTime = now
          , bTCUSDCentRateCentsPerBTC = currRate
          }
        return ()
    Nothing -> do -- there are no rates, refreshing
          currRate <- getCurrentUSDCentsPerBTC config
          _ <- flip runSqlPersistMPool pool $ insert $ BTCUSDCentRate
            { bTCUSDCentRateCreationTime = now
            , bTCUSDCentRateCentsPerBTC = currRate
            }
          return ()
  where
    daySecs = 24 * 3600

schedulerMain :: Config-> Pool SqlBackend-> IO ()
schedulerMain config pool = do
  getCurrentTime >>= loop
  where
    secondsSince now lastCheck = verifyNatural $ floor $! nominalDiffTimeToSeconds $ now `diffUTCTime` lastCheck
    loop state@(lastDepositCheck) = do
      now <- getCurrentTime
      ensureAvailableWalletsPool config pool wallet
      let depositCheckNeeded = ( now `secondsSince` lastDepositCheck >= naturalFromPositive ( configBTCDepositCheckPeriodSecs config))
          newLastDepositCheck = if depositCheckNeeded then now else lastDepositCheck
      print $ "lastDepositCheck " <> show lastDepositCheck <> ", depositCheckNeeded " <> show depositCheckNeeded
      hFlush stdout
      when depositCheckNeeded $ do
        checkForDeposits config pool
        checkForWithdraws now config pool
      threadDelay (configLoopSleepUSec config)
      loop newLastDepositCheck
    wallet = BTC.Wallet (configBTCWallet config)

lockWallet :: Config-> Pool SqlBackend-> PersonId-> Positive USDCents-> IO BTCAddress
lockWallet config pool personId usdCents = do
  now <- getCurrentTime
  env <- BTC.mkClientEnv <$> newManager defaultManagerSettings <*> return (configBTCURL config)
  mLockedWallet <- flip runSqlPersistMPool pool $ do
    mfreeWallet <- selectFirst ([ ] :: [Filter BTCWalletFree]) [ ]
    case mfreeWallet of
      Nothing-> return Nothing
      Just (Entity freeWalletId freeWallet) -> do
        ebitcoin <- liftIO $ flip runClientM env $ BTC.getReceivedByAddress userPass wallet ((bTCWalletFreeAddress freeWallet), verifyNatural 6)
        case ebitcoin of
          Right (Result _ bitcoin) -> do
            currentUSDCentsPerBTC <- liftIO $ getStoredUSDCentsPerBTC config pool
            let currentSatoshis = BTC.bitcoinToSatoshi bitcoin
            insert $ BTCWalletLock
              { bTCWalletLockAddress = bTCWalletFreeAddress freeWallet
              , bTCWalletLockBalanceAtLock = currentSatoshis
              , bTCWalletLockBalanceAtLastCheck = currentSatoshis
              , bTCWalletLockCreationTime = bTCWalletFreeCreationTime freeWallet
              , bTCWalletLockLockTime = now
              , bTCWalletLockExpectedDeposit = usdCents
              , bTCWalletLockPersonId = personId
              }
            delete freeWalletId
            return $ Just $! bTCWalletFreeAddress freeWallet
          Left some-> error $ "lockWallet: bitcoin error" <> show some
  case mLockedWallet of
    Nothing-> do -- there are no free wallets available: fix and retry
      ensureAvailableWalletsPool config pool wallet
      lockWallet config pool personId usdCents
    Just lockedWallet-> return lockedWallet
  

  where
    userPass = BasicAuthData (T.encodeUtf8 $ configBTCUser config) (T.encodeUtf8 $ configBTCPassword config)
    wallet = BTC.Wallet (configBTCWallet config)

withdraw :: Hash-> Config-> Pool SqlBackend-> PersonId-> BTCWithdrawRequest-> IO (Either T.Text WithdrawResponse)
withdraw uuid config pool personId req = do
  now <- getCurrentTime
  flip runSqlPersistMPool pool $ do
    person <- get personId >>= return . fromJust
    let positiveWithdrawUSCents = naturalFromPositive $! btcWithdrawRequestUSDCents req
    if personAvailableBalance person < positiveWithdrawUSCents
      then return $! Left "not enough balance"
      else do
        currentUSDCentsPerBTC <- liftIO $ getStoredUSDCentsPerBTC config pool
        insert $ BtcWithdrawQueue
          { btcWithdrawQueuePersonId = personId
          , btcWithdrawQueueCreationTime = now
          , btcWithdrawQueueUsdAmount = btcWithdrawRequestUSDCents req
          , btcWithdrawQueueDestination = btcWithdrawRequestAddress req
          , btcWithdrawQueueSessionId = uuid
          }
        update personId
          [ PersonAvailableBalance =. personAvailableBalance person - positiveWithdrawUSCents
          , PersonWithdrawLockedBalance =. personWithdrawLockedBalance person + positiveWithdrawUSCents
          ]
        insert $ TransactionLog
          { transactionLogTransactionType = WithdrawRequested
          , transactionLogCurrency = BTC
          , transactionLogUsdCentsCurrencyRate = currentUSDCentsPerBTC
          , transactionLogPersonId = personId
          , transactionLogUsdAmount = btcWithdrawRequestUSDCents req
          , transactionLogTime = now
          , transactionLogComment = Nothing
          , transactionLogTechComment = Nothing
          }
        return $! Right WithdrawQueued

checkForWithdraws :: UTCTime-> Config -> Pool SqlBackend-> IO ()
checkForWithdraws now config pool = handle handleSendToAddressError $ do
  now <- getCurrentTime
  flip runSqlPersistMPool pool $ do
    runConduit $ selectSource ([ ] :: [Filter BtcWithdrawQueue]) [] .| withdrawOrQueueForConfirmation .| actualWithdraw .| queueWithdrawForConfirmation .| (C.awaitForever (\_-> return ()))
  where
    wallet = BTC.Wallet (configBTCWallet config)
    userPass = BasicAuthData (T.encodeUtf8 $ configBTCUser config) (T.encodeUtf8 $ configBTCPassword config)
    actualWithdraw = C.awaitForever $ \epair->
      case epair of
        Left some -> C.yield $ Left some
        Right (queueId, queue)-> do
          now <- liftIO getCurrentTime
          liftIO $ do
            print "performing automaticall withdrawal"
            hFlush stdout
          usdCentsRate <- liftIO $! getStoredUSDCentsPerBTC config pool
          let bitcoins = BTC.satoshiToBitcoin $! usdCentsAsSatoshis usdCentsRate $! naturalFromPositive (btcWithdrawQueueUsdAmount queue)
          lift $ do -- persist withdrawal in our DB
            person <- get (btcWithdrawQueuePersonId queue) >>= pure . fromJust
            delete queueId
            liftIO $ do
              print "deleted queueId"
              hFlush stdout
            update (btcWithdrawQueuePersonId queue)
              [ PersonWithdrawSum =. (personWithdrawSum person) + (naturalFromPositive (btcWithdrawQueueUsdAmount queue))
              , PersonWithdrawLockedBalance =. (personWithdrawLockedBalance person) - (naturalFromPositive (btcWithdrawQueueUsdAmount queue))
              ]
            liftIO $ do
              print "updated balance"
              hFlush stdout
            transactionLogId <- insert $ TransactionLog
              { transactionLogTransactionType = Withdraw
              , transactionLogCurrency = BTC
              , transactionLogPersonId = (btcWithdrawQueuePersonId queue)
              , transactionLogUsdCentsCurrencyRate = usdCentsRate
              , transactionLogTime = now
              , transactionLogUsdAmount = btcWithdrawQueueUsdAmount queue
              , transactionLogComment = Nothing
              , transactionLogTechComment = Nothing
              }
            liftIO $ do
              print "created transaction log"
              hFlush stdout
            ebtcTransactionId <- liftIO $! do -- perform bitcoin rpc call
              print $! "automatically sending bitcoins " <> show bitcoins <> " to address " <> (show $!  btcWithdrawQueueDestination queue)
              hFlush stdout
              BTC.withBitcoin (configBTCURL config) $! BTC.sendToAddress userPass wallet ((btcWithdrawQueueDestination queue), positiveFromNatural bitcoins)
            case ebtcTransactionId of
              Result _ btcTransactionId -> do -- persist bitcoin transaction as transactionlog comment
                liftIO $ do
                  print $ "sucessfully created transaction " <> btcTransactionId
                  hFlush stdout
                update transactionLogId [ TransactionLogComment =. Just btcTransactionId ]
              some -> do
                liftIO $ do
                  print $ "send to address failed" <> show some
                  hFlush stdout
                liftIO $ E.ioError $ E.userError $ "sendToAddress error: bitcoin client error detected for sessionId " <> (show $ btcWithdrawQueueSessionId queue) <> ", person: " <> (show  $ personReferId person ) <> ". depositSum: " <> (show $ personDepositSum person) <> ", withdraw sum: " <> (show $ personWithdrawSum person) <> ", requested withdraw: " <> (show $ btcWithdrawQueueUsdAmount queue) <> ", withdraw destination: " <> (show $ btcWithdrawQueueDestination queue) <> ", error: " <> show some
    handleSendToAddressError:: SomeException-> IO ()
    handleSendToAddressError e = do
      print $ "send to address failed " <> show e
      hFlush stdout
      now <- getCurrentTime
      _ <- flip runSqlPersistMPool pool $ do
        insert $! AdminNotification
          { adminNotificationCreationTime = now
          , adminNotificationNotification = tshow e
          }
      return ()
    queueWithdrawForConfirmation = C.awaitForever $ \epair->
      case epair of
        Right some -> C.yield $ Right some
        Left (queueId, queue)-> lift $ do
          now <- liftIO getCurrentTime
          liftIO $! do
            print $ "enqueueing withdrawal " <> show queue <> " for admin confirmation"
            hFlush stdout
          person <- get (btcWithdrawQueuePersonId queue) >>= pure . fromJust
          insert $! AdminNotification
            { adminNotificationCreationTime = now
            , adminNotificationNotification = "confirmation is needed for BTC withdrawal id " <> (tshow $ btcWithdrawQueueSessionId queue) <>  " for user " <> (tshow  $ personReferId person ) <> ". depositSum: " <> (tshow $ personDepositSum person) <> ", withdraw sum: " <> (tshow $ personWithdrawSum person) <> ", requested withdraw: " <> (tshow $ btcWithdrawQueueUsdAmount queue) <> ", withdraw destination: " <> (tshow $ btcWithdrawQueueDestination queue)
            }
          insert $! BtcWithdrawConfirmationQueue
            { btcWithdrawConfirmationQueueCreationTime = now
            , btcWithdrawConfirmationQueueUsdAmount = btcWithdrawQueueUsdAmount queue
            , btcWithdrawConfirmationQueuePersonId = btcWithdrawQueuePersonId queue
            , btcWithdrawConfirmationQueueDestination = btcWithdrawQueueDestination queue
            , btcWithdrawConfirmationQueueSessionId = btcWithdrawQueueSessionId queue
            }
          delete queueId

    withdrawOrQueueForConfirmation = C.awaitForever $ \(Entity queueId queue) -> do
      liftIO $ do
        print "withdraw record:"
        print queue
        hFlush stdout
      person <- lift $ get (btcWithdrawQueuePersonId queue) >>= pure . fromJust
      liftIO $ do
        print "(personWithdrawSum person) + (naturalFromPositive $ btcWithdrawQueueUsdAmount queue) <= (personDepositSum person) * 2"
        print $ show (personWithdrawSum person) <> " + " <> show (naturalFromPositive $ btcWithdrawQueueUsdAmount queue) <> " <= " <> show ((personDepositSum person) * 2)
        hFlush stdout
      if (personWithdrawSum person) + (naturalFromPositive $ btcWithdrawQueueUsdAmount queue) <= (personDepositSum person) * 2
        then do
          liftIO $ do
            print "withdrawal will be automaticall"
            hFlush stdout
          C.yield $ Right (queueId, queue)
        else do
          liftIO $ do
            print "withdrawal will not be automaticall"
            hFlush stdout
          C.yield $ Left (queueId, queue)

withdrawWaitingForConfirm :: Config-> Pool SqlBackend-> IO [BtcWaitingToConfirmRequest]
withdrawWaitingForConfirm confg pool = flip runSqlPersistMPool pool $ do
  runConduit $ selectSource ([ ] :: [Filter BtcWithdrawConfirmationQueue]) []
    .| requestToBtcWaitingToConfirmRequest
    .| C.consume
  where
    requestToBtcWaitingToConfirmRequest = C.awaitForever $ \(Entity queueId record) -> do
      person <- lift $ (get $ btcWithdrawConfirmationQueuePersonId record) >>= return . fromJust
      C.yield $ BtcWaitingToConfirmRequest
        { btcWaitingToConfirmRequestUUID = btcWithdrawConfirmationQueueSessionId record
        , btcWaitingToConfirmRequestCreationTime = btcWithdrawConfirmationQueueCreationTime record
        , btcWaitingToConfirmRequestPerson = personReferId person
        , btcWaitingToConfirmRequestDepositSum = personDepositSum person
        , btcWaitingToConfirmRequestWithdrawSum = personWithdrawSum person
        , btcWaitingToConfirmRequestWithdrawRequested = btcWithdrawConfirmationQueueUsdAmount record
        }
  
withdrawDecline :: Config-> Pool SqlBackend-> Hash-> AdminId -> IO ()
withdrawDecline config pool session adminId = do
  currentUSDCentsPerBTC <- getStoredUSDCentsPerBTC config pool
  now <- getCurrentTime
  flip runSqlPersistMPool pool $ do
    mrecord <- selectFirst [ BtcWithdrawConfirmationQueueSessionId ==. session ] []
    case mrecord of
      Nothing-> return ()
      Just (Entity recordId record ) -> do
        liftIO $ do
          print $ "declining withdraw confirmation " <> show record
          hFlush stdout
        -- delete withdraw queue
        delete recordId
        person <- (get $ btcWithdrawConfirmationQueuePersonId record) >>= return . fromJust
        -- mark previously locked balance as available
        update (btcWithdrawConfirmationQueuePersonId record)
          [ PersonWithdrawLockedBalance =. personWithdrawLockedBalance person - (naturalFromPositive $ btcWithdrawConfirmationQueueUsdAmount record)
          , PersonAvailableBalance =. personAvailableBalance person + (naturalFromPositive $ btcWithdrawConfirmationQueueUsdAmount record)
          ]
        insert $ TransactionLog
          { transactionLogTransactionType = WithdrawDeclined
          , transactionLogCurrency = BTC
          , transactionLogUsdCentsCurrencyRate = currentUSDCentsPerBTC
          , transactionLogPersonId = btcWithdrawConfirmationQueuePersonId record
          , transactionLogUsdAmount = btcWithdrawConfirmationQueueUsdAmount record
          , transactionLogTime = now
          , transactionLogComment = Nothing
          , transactionLogTechComment = Just $ "admin id " <> (tshow $ adminId)
          }
        return ()


withdrawConfirm :: Config-> Pool SqlBackend-> Hash-> AdminId -> IO ()
withdrawConfirm config pool session adminId = handle (handleSendToAddressError session) $ do
  now <- getCurrentTime
  flip runSqlPersistMPool pool $ do
    mrecord <- selectFirst [ BtcWithdrawConfirmationQueueSessionId ==. session ] []
    case mrecord of
      Nothing-> return ()
      Just (Entity recordId record) -> do
        liftIO $ do
          print $ "withdrawal " <> show session <> " confirmed by admin " <> show adminId
          hFlush stdout
        usdCentsRate <- liftIO $! getStoredUSDCentsPerBTC config pool
        let bitcoins = BTC.satoshiToBitcoin $! usdCentsAsSatoshis usdCentsRate $! naturalFromPositive (btcWithdrawConfirmationQueueUsdAmount record)
        person <- get (btcWithdrawConfirmationQueuePersonId record) >>= pure . fromJust
        delete recordId
        liftIO $ do
          print $ "deleted recordId" <> show recordId
          hFlush stdout
        update (btcWithdrawConfirmationQueuePersonId record)
          [ PersonWithdrawSum =. (personWithdrawSum person) + (naturalFromPositive (btcWithdrawConfirmationQueueUsdAmount record))
          , PersonWithdrawLockedBalance =. (personWithdrawLockedBalance person) - (naturalFromPositive (btcWithdrawConfirmationQueueUsdAmount record))
          ]
        liftIO $ do
          print "updated balance"
          hFlush stdout
        transactionLogId <- insert $ TransactionLog
          { transactionLogTransactionType = Withdraw
          , transactionLogCurrency = BTC
          , transactionLogPersonId = (btcWithdrawConfirmationQueuePersonId record)
          , transactionLogUsdCentsCurrencyRate = usdCentsRate
          , transactionLogTime = now
          , transactionLogUsdAmount = btcWithdrawConfirmationQueueUsdAmount record
          , transactionLogComment = Nothing
          , transactionLogTechComment = Just $ tshow adminId
          }
        liftIO $ do
          print "created transaction log"
          hFlush stdout
        ebtcTransactionId <- liftIO $! do -- perform bitcoin rpc call
          print $! "confirming sending bitcoins " <> show bitcoins <> " to address " <> (show $!  btcWithdrawConfirmationQueueDestination record)
          hFlush stdout
          BTC.withBitcoin (configBTCURL config) $! BTC.sendToAddress userPass wallet ((btcWithdrawConfirmationQueueDestination record), positiveFromNatural bitcoins)
        case ebtcTransactionId of
          Result _ btcTransactionId -> do -- persist bitcoin transaction as transactionlog comment
            liftIO $ do
              print $ "sucessfully created transaction " <> btcTransactionId
              hFlush stdout
            update transactionLogId [ TransactionLogComment =. Just btcTransactionId ]
          some -> do
            liftIO $ do
              print $ "send to address failed" <> show some
              hFlush stdout
            liftIO $ E.ioError $ E.userError $ "sendToAddress error: bitcoin client error detected for sessionId " <> (show $ btcWithdrawConfirmationQueueSessionId record) <> ", person: " <> (show  $ personReferId person ) <> ". depositSum: " <> (show $ personDepositSum person) <> ", withdraw sum: " <> (show $ personWithdrawSum person) <> ", requested withdraw: " <> (show $ btcWithdrawConfirmationQueueUsdAmount record) <> ", withdraw destination: " <> (show $ btcWithdrawConfirmationQueueDestination record) <> ", error: " <> show some

  where
    wallet = BTC.Wallet (configBTCWallet config)
    userPass = BasicAuthData (T.encodeUtf8 $ configBTCUser config) (T.encodeUtf8 $ configBTCPassword config)
    handleSendToAddressError:: Hash-> SomeException-> IO ()
    handleSendToAddressError session e = do
      print $ "send to address failed " <> show e
      hFlush stdout
      now <- getCurrentTime
      _ <- flip runSqlPersistMPool pool $ do
        insert $! AdminNotification
          { adminNotificationCreationTime = now
          , adminNotificationNotification = "failed to process btc withdraw confirmation with UUID " <> tshow session <> ", error: " <> tshow e
          }
      return ()
