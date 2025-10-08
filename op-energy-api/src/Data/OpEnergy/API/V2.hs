{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.API.V2 where

import           Servant.API
import           Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as Aeson
import           Control.Applicative ((<|>))
import           GHC.Generics
import           Data.Swagger (ToSchema(..))
import           Data.Typeable (Typeable)
import           Data.Proxy (Proxy(..))

import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.API.V1.Block
import qualified Data.OpEnergy.API.V1 as V1
import           Data.OpEnergy.API.Tags

-- | Response type that can be either a summary or full headers
data BlockSpanResponse
  = BlockSpanSummaryResponse V1.BlockSpanSummary
  | BlockSpanFullResponse V1.BlockSpanHeadersNbdrHashrate
  deriving (Show, Generic, Typeable)

instance ToJSON BlockSpanResponse where
  toJSON (BlockSpanSummaryResponse summary) = toJSON summary
  toJSON (BlockSpanFullResponse full) = toJSON full

instance FromJSON BlockSpanResponse where
  parseJSON v = (BlockSpanFullResponse <$> Aeson.parseJSON v) <|> (BlockSpanSummaryResponse <$> Aeson.parseJSON v)

instance ToSchema BlockSpanResponse where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy V1.BlockSpanHeadersNbdrHashrate)

type V2API = Tags "Blockspans" :> V2APIEndpoints

-- | API specifications of a backend service for Swagger
type V2APIEndpoints
  = "statistics"
    :> Capture "blockheight" BlockHeight
    :> Capture "spansize" (Positive Int)
    :> Description "Derpecated? Calculates NBDR statistics for a given block height and span. NBDR here is ratio (span * 600 * 100) / (endBlockMedianTime - startBlockMediantime)."
    :> Get '[JSON] V1.Statistics

  :<|> "block"
    :> Capture "hash" BlockHash
    :> Description "Returns block's header by a given block hash, including chainwork, that is missing from mempool's blocks' headers cache"
    :> Get '[JSON] BlockHeader

  :<|> "blockbyheight"
    :> Capture "height" BlockHeight
    :> Description "Returns block's header by a given block height"
    :> Get '[JSON] BlockHeader

  :<|> "blockswithnbdrbyblockspan"
    :> Capture "startBlockHeight" BlockHeight
    :> Capture "spansize" (Positive Int)
    :> QueryParam "numberOfSpan" (Positive Int)
    :> Description "DEPRECATED. use blockbyblockspan instead. Returns list of start and end blocks' headers and their nbdr for each appropriate block span. NBDR here is ratio (spansize * 600 * 100) / (endBlockMedianTime - startBlockMediantime). If numberOfSpan is missing, then it will provide blockspans until the current tip."
    :> Get '[JSON] [V1.BlockSpanHeadersNbdr]

  :<|> "blockspans"
    :> Capture "startBlockHeight" BlockHeight
    :> Capture "spansize" (Positive Int)
    :> QueryParam "numberOfSpans" (Positive Int)
    :> QueryParam "withHeaders" Bool
    :> Description "Returns a numberOfSpans sized array of blockspans, with same options as 'blockspan' (single blockspan) for each blockspan. If numberOfSpans is not given, then it will provide as many blockspans as possible with size 'spansize' until the current tip. If withHeaders is not given or true, returns full block headers; if false, returns summary with just heights, nbdr, and hashrate."
    :> Get '[JSON] [BlockSpanResponse]

  :<|> "blockspan"
    :> Capture "blockHeight" BlockHeight
    :> QueryParam "spanSize" (Positive Int)
    :> Description "Returns a single blockspan ending at the specified block height. A blockspan is the start and end block for a blockspan, along with summary data (NBDR and hashrate) and optional header infos for blocks. spanSize defaults to 24 if not specified."
    :> Get '[JSON] V1.BlockSpanHeadersNbdrHashrate

  :<|> "git-hash"
    :> Description "returns short hash of commit of the op-energy git repo that had been used to build backend"
    :> Get '[JSON] V1.GitHashResponse





