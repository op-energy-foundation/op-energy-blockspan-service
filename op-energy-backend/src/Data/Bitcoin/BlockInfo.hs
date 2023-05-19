{-- |
 - exports BlockInfo data type
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Data.Bitcoin.BlockInfo where

import           GHC.Generics(Generic)
import           Data.Aeson
import           Data.Word
import           Data.Int(Int32)

import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Hash

data BlockInfo = BlockInfo
  { hash :: BlockHash
  , confirmations :: Natural Int
  , height :: BlockHeight
  , version :: Int32
  , merkleroot :: Hash
  , time :: Word32
  , mediantime :: Word32
  , nonce :: Word32
  , bits :: Bits
  , difficulty :: Double
  , chainwork :: Natural Integer
  , nTx :: Word64
  , previousblockhash :: Maybe BlockHash
  , nextblockhash :: Maybe BlockHash
  , strippedsize :: Word64
  , size :: Word64
  , weight :: Word64
  }
  deriving (Eq, Show, Generic)
instance ToJSON   BlockInfo
instance FromJSON BlockInfo

