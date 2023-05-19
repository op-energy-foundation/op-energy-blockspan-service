{-- |
 - exports BlockStats data type
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}

module Data.Bitcoin.BlockStats where

import           GHC.Generics(Generic)
import           Data.Aeson
import           Data.Word
import           Data.Int

import           Data.OpEnergy.API.V1.Block

data BlockStats = BlockStats
  { avgfee :: Word64 -- ^ ./src/amount.h:typedef int64_t CAmount
  , avgfeerate :: Word64
  , avgtxsize :: Word64
  , blockhash :: BlockHash
  , feerate_percentiles :: [Word64]
  , height :: BlockHeight
  , ins :: Word64
  , maxfee :: Word64
  , maxfeerate :: Word64
  , maxtxsize :: Word64
  , medianfee :: Word64
  , mediantxsize :: Word64
  , minfee :: Word64
  , minfeerate :: Word64
  , mintxsize :: Word64
  , outs :: Word64
  , subsidy :: Word64
  , swtotal_size :: Word64
  , swtotal_weight :: Word64
  , swtxs :: Word64 -- ^ ./src/rpc/blockchain.cpp:    int64_t swtxs
  , total_out :: Word64
  , total_size :: Word64
  , total_weight :: Word64
  , totalfee :: Word64
  , utxo_increase :: Int64
  , utxo_size_inc :: Int64
  }
  deriving (Eq, Show, Generic)
instance ToJSON   BlockStats
instance FromJSON BlockStats

