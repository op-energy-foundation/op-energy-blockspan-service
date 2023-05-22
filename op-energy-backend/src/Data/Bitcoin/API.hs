{-- | This module exports API and client functions of Bitcoin JSON-RPC
 - There are some bitcoin rpc libraries on hackage, but there were some
 - issues with wallets and those libraries are HTTP. bitcoin-cli is using
 - JSON-RPC, so I made a decision to make a small JSON-RPC library.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}

module Data.Bitcoin.API where

import           Data.Proxy              (Proxy (..))
import           Network.HTTP.Client     as Client(defaultManagerSettings, newManager, Manager, Request(..)
                                                  )
import           Servant.API             ((:<|>) (..) )
import           Servant.Client          ( ClientM, client, mkClientEnv
                                          , BaseUrl, ClientEnv, makeClientRequest, runClientM)
import           Servant.Client.JsonRpc  (JsonRpcResponse)


import           Servant.API     ((:>), BasicAuth, BasicAuthData, Capture, ToHttpApiData(..))
import           Servant.JsonRpc (JsonRpc, RawJsonRpc)
import           Data.Text(Text)
import           GHC.Generics(Generic)
import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import           Data.Word
import qualified Control.Monad.Fail as Fail
import           Control.Monad.IO.Class(liftIO)

import           Data.Bitcoin.BlockStats
import           Data.Bitcoin.BlockInfo
import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1.Block

type BTCAddress = Text

newtype Wallet = Wallet Text
  deriving (Eq,Show, Generic)
instance ToJSON Wallet
instance FromJSON Wallet where
  parseJSON = withText "wallet" $ \v -> return (Wallet v)
instance ToHttpApiData Wallet where
  toUrlPiece (Wallet t) = toUrlPiece t
  toQueryParam (Wallet t) = toQueryParam t

data LoadWalletResponse = LoadWalletResponse
  { name :: Wallet
  , warning :: Text
  }
  deriving (Eq, Show, Generic)
instance ToJSON LoadWalletResponse
instance FromJSON LoadWalletResponse

type Satoshi = Natural Int
newtype Bitcoin = Bitcoin Satoshi
  deriving (Eq, Show, Ord, Generic)
instance Num Bitcoin where
  (+) (Bitcoin left) (Bitcoin right) = Bitcoin (left + right)
  (-) (Bitcoin left) (Bitcoin right) = Bitcoin (left - right)
  (*) (Bitcoin left) (Bitcoin right) = Bitcoin (left * right)
  abs v = v
  signum (Bitcoin left) = Bitcoin (signum left)
  fromInteger v = Bitcoin (fromInteger v)
instance ToJSON (Positive Bitcoin) where
  toJSON (Positive satoshis) = toJSON ( (fromIntegral satoshis) / (100000000.0::Double))
instance FromJSON (Positive Bitcoin) where
  parseJSON = withScientific "Bitcoin" $ \s-> return $! verifyPositive (s * 100000000)
instance ToJSON (Natural Bitcoin) where
  toJSON (Natural (Bitcoin (Natural satoshis))) = toJSON ( (fromIntegral satoshis) / (100000000.0::Double))
instance FromJSON (Natural Bitcoin) where
  parseJSON = withScientific "Bitcoin" $ \s-> return $! verifyNatural $ Bitcoin $ verifyNaturalScientific (s * 100000000)


data ReceivedByAddress = ReceivedByAddress
  { address :: BTCAddress
  , amount :: Natural Bitcoin
  , confirmations :: Natural Int
  , label :: Text
  , txids :: [ Text]
  }
  deriving (Eq, Show, Generic)
instance ToJSON ReceivedByAddress
instance FromJSON ReceivedByAddress

data BlockchainInfo = BlockchainInfo
  { chain:: Text
  , blocks:: Natural Int
  , headers:: Natural Int
  , bestblockhash:: BlockHash
  , difficulty:: Double
  , time:: Maybe Word32 -- Looks like this field is absent when bitcoin node has only base block (0)
  , mediantime:: Word32
  , verificationprogress:: Double
  , initialblockdownload:: Bool
  , chainwork:: Natural Integer
  , size_on_disk:: Word64
  , pruned:: Bool
  , pruneheight:: Maybe (Natural Int)
  , automatic_pruning:: Maybe Bool
  , prune_target_size:: Maybe Word64
  , warnings:: Text
  }
  deriving (Eq, Show, Generic)
instance ToJSON BlockchainInfo
instance FromJSON BlockchainInfo

type ListWallets = BasicAuth "auth" (Text, Text) :> JsonRpc "listwallets" [Int] String [Wallet]
type CreateWallet = BasicAuth "auth" (Text, Text) :> JsonRpc "createwallet" [Text] String LoadWalletResponse
type LoadWallet = BasicAuth "auth" (Text, Text) :> JsonRpc "loadwallet" [Text] String LoadWalletResponse
type GetNewAddress = BasicAuth "auth" (Text, Text) :> "wallet" :> Capture "wallet" Wallet :> JsonRpc "getnewaddress" [Int] String BTCAddress
type ListReceivedByAddress = BasicAuth "auth" (Text, Text) :> "wallet" :> Capture "wallet" Wallet :> JsonRpc "listreceivedbyaddress" (Int, Bool) String [ReceivedByAddress]
type GetReceivedByAddress = BasicAuth "auth" (Text, Text) :> "wallet" :> Capture "wallet" Wallet :> JsonRpc "getreceivedbyaddress" (BTCAddress, Natural Int) String (Natural Bitcoin)
type SendToAddress = BasicAuth "auth" (Text, Text) :> "wallet" :> Capture "wallet" Wallet :> JsonRpc "sendtoaddress" (BTCAddress, Positive Bitcoin) String Text
type GetBlockchainInfo = BasicAuth "auth" (Text, Text) :> JsonRpc "getblockchaininfo" [Int] String BlockchainInfo
type GetBlockStats = BasicAuth "auth" (Text, Text) :> JsonRpc "getblockstats" [BlockHeight] String BlockStats
type GetBlock = BasicAuth "auth" (Text, Text) :> JsonRpc "getblock" [BlockHash] String BlockInfo
type GetBlockHash = BasicAuth "auth" (Text, Text) :> JsonRpc "getblockhash" [BlockHeight] String BlockHash

type RpcAPI
  = ListWallets
  :<|> CreateWallet
  :<|> LoadWallet
  :<|> GetNewAddress
  :<|> ListReceivedByAddress
  :<|> GetReceivedByAddress
  :<|> SendToAddress
  :<|> GetBlockchainInfo
  :<|> GetBlockStats
  :<|> GetBlock
  :<|> GetBlockHash


type JsonRpcAPI = RawJsonRpc RpcAPI


type API = JsonRpcAPI

listWallets :: BasicAuthData-> [Int] -> ClientM (JsonRpcResponse String [Wallet])
createWallet :: BasicAuthData-> [Text] -> ClientM (JsonRpcResponse String LoadWalletResponse)
loadWallet :: BasicAuthData-> [Text] -> ClientM (JsonRpcResponse String LoadWalletResponse)
getNewAddress :: BasicAuthData-> Wallet-> [Int] -> ClientM (JsonRpcResponse String BTCAddress)
listReceivedByAddress :: BasicAuthData-> Wallet-> (Int, Bool) -> ClientM (JsonRpcResponse String [ReceivedByAddress])
getReceivedByAddress :: BasicAuthData-> Wallet-> (BTCAddress, Natural Int) -> ClientM (JsonRpcResponse String (Natural Bitcoin))
sendToAddress :: BasicAuthData-> Wallet-> (BTCAddress, Positive Bitcoin)-> ClientM (JsonRpcResponse String Text)
getBlockchainInfo :: BasicAuthData-> [Int] -> ClientM (JsonRpcResponse String BlockchainInfo)
getBlockStats :: BasicAuthData-> [BlockHeight] -> ClientM (JsonRpcResponse String BlockStats)
getBlock :: BasicAuthData-> [BlockHash] -> ClientM (JsonRpcResponse String BlockInfo)
getBlockHash :: BasicAuthData-> [BlockHeight] -> ClientM (JsonRpcResponse String BlockHash)
listWallets :<|> createWallet :<|> loadWallet :<|> getNewAddress :<|> listReceivedByAddress :<|> getReceivedByAddress :<|> sendToAddress :<|> getBlockchainInfo :<|> getBlockStats :<|> getBlock :<|> getBlockHash = client $ Proxy @API


mkClientEnv :: Client.Manager -> BaseUrl -> ClientEnv
mkClientEnv mgr burl = env { makeClientRequest = newMakeClientRequest}
  where
    env = Servant.Client.mkClientEnv mgr burl
    newMakeClientRequest burl req = ((makeClientRequest env) burl req) { Client.queryString = BS.empty}


withBitcoin :: BaseUrl-> ClientM a-> IO a
withBitcoin url payload = do
  env <- Data.Bitcoin.API.mkClientEnv <$> newManager defaultManagerSettings <*> return url
  eresult <- flip runClientM env payload
  case eresult of
    Left some -> error $ "withBitcoin: error: " <> show some
    Right some -> return some

instance Fail.MonadFail ClientM where
  fail s = liftIO $ fail s
