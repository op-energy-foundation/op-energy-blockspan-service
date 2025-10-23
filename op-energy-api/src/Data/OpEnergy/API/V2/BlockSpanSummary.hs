{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.API.V2.BlockSpanSummary
  ( BlockSpanSummary(..)
  ) where

import           GHC.Generics

import           Control.Lens
import           Data.Aeson (ToJSON(..), FromJSON(..))
import           Data.Swagger

import           Data.OpEnergy.API.V1.Natural

data BlockSpanSummary = BlockSpanSummary
  { startBlockHeight :: Natural Int
  , endBlockHeight :: Natural Int
  , nbdr :: Double
  , hashrate :: Natural Integer
  }
  deriving (Show, Generic)
instance ToJSON   BlockSpanSummary
instance FromJSON BlockSpanSummary
instance ToSchema BlockSpanSummary where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "BlockSpanSummary schema"
    & mapped.schema.example ?~ toJSON defaultBlockSpanSummary
defaultBlockSpanSummary :: BlockSpanSummary
defaultBlockSpanSummary = BlockSpanSummary
  { startBlockHeight = 100000
  , endBlockHeight = 100024
  , nbdr = 100.0
  , hashrate = 1000000
  }

