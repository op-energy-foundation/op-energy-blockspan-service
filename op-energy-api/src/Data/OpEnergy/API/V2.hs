{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.API.V2 where

import           Servant.API hiding (Header)
import qualified Servant.API as Servant (Header)
import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Natural
import qualified Data.OpEnergy.API.V1 as V1

-- | API specifications of a backend service for Swagger  
-- Legacy V1 endpoints (now deprecated in V2 namespace)
type V2API
  = "block"
    :> Capture "hash" BlockHash
    :> Description "Returns block's header by a given block hash, including chainwork, that is missing from mempool's blocks' headers cache"
    :> Get '[JSON] BlockHeader

  :<|> "blockbyheight"
    :> Capture "height" BlockHeight
    :> Description "Returns block's header by a given block height"
    :> Get '[JSON] BlockHeader

  :<|> "blocksbyblockspan"
    :> Capture "startBlockHeight" BlockHeight
    :> Capture "spansize" (Positive Int)
    :> QueryParam "numberOfSpans" (Positive Int)
    :> QueryParam "withNBDR" Bool
    :> QueryParam "withHashrate" Bool
    :> Description "DEPRECATED. Use /blockspans/blockspans instead. Returns list of blocks' headers by a given block span. NBDR here is ratio (spansize * 600 * 100) / (endBlockMedianTime - startBlockMediantime). Hashrate here is a ratio (endBlockChainwork - startBlockChainwork) / (endBlockMedianTime - startBlockMediantime). If numberOfSpans is missing, then it will provide blockspans until the current tip."
    :> Get '[JSON] (Headers '[Servant.Header "Deprecation" String, Servant.Header "Sunset" String, Servant.Header "Link" String] [V1.BlockSpanHeadersNbdrHashrate])

  :<|> "blockswithnbdrbyblockspan"
    :> Capture "startBlockHeight" BlockHeight
    :> Capture "spansize" (Positive Int)
    :> QueryParam "numberOfSpans" (Positive Int)
    :> Description "DEPRECATED. Use /blockspans/blockspans instead. Returns list of start and end blocks' headers and their nbdr for each appropriate block span. NBDR here is ratio (spansize * 600 * 100) / (endBlockMedianTime - startBlockMediantime). If numberOfSpans is missing, then it will provide blockspans until the current tip."
    :> Get '[JSON] (Headers '[Servant.Header "Deprecation" String, Servant.Header "Sunset" String, Servant.Header "Link" String] [V1.BlockSpanHeadersNbdr])

  :<|> "blockswithhashratebyblockspan"
    :> Capture "startBlockHeight" BlockHeight
    :> Capture "spansize" (Positive Int)
    :> QueryParam "numberOfSpans" (Positive Int)
    :> Description "DEPRECATED. Use /blockspans/blockspans instead. Returns list of start and end blocks' headers and their hashrate for each appropriate block span. Hashrate here is a ratio (endBlockChainwork - startBlockChainwork) / (endBlockMedianTime - startBlockMediantime). If numberOfSpans is missing, then it will provide blockspans until the current tip."
    :> Get '[JSON] (Headers '[Servant.Header "Deprecation" String, Servant.Header "Sunset" String, Servant.Header "Link" String] [V1.BlockSpanHeadersHashrate])

  :<|> "blockspanlist"
    :> Capture "startBlockHeight" BlockHeight
    :> Capture "spansize" (Positive Int)
    :> Capture "numberOfSpans" (Positive Int)
    :> Description "DEPRECATED. Use /blockspans/blockspans instead. Returns list of blockspans started from startBlockHeight of size span and numberOfSpans length"
    :> Get '[JSON] (Headers '[Servant.Header "Deprecation" String, Servant.Header "Sunset" String, Servant.Header "Link" String] [BlockSpan])

  :<|> "git-hash"
    :> Description "returns short hash of commit of the op-energy git repo that had been used to build backend"
    :> Get '[JSON] V1.GitHashResponse

  -- New V2 Blockspan endpoints following document specification
  :<|> "blockspans" :> "blockspan" 
    :> Capture "blockHeight" BlockHeight
    :> QueryParam "spansize" (Positive Int)
    :> QueryParam "withHeaderInfos" Bool
    :> Description "Returns a single blockspan ending at the specified block height. Always includes nbdr and hashrate values. spansize defaults to 24 if not provided."
    :> Get '[JSON] BlockSpanV2

  :<|> "blockspans" :> "blockspans"
    :> Capture "startBlockHeight" BlockHeight
    :> Capture "numberOfSpans" (Positive Int)
    :> QueryParam "spansize" (Positive Int)
    :> QueryParam "withHeaders" Bool
    :> Description "Returns numberOfSpans sized array of blockspans. Always includes nbdr and hashrate values. spansize defaults to 24 if not provided. If numberOfSpans is not given in implementation, provides blockspans until current tip."
    :> Get '[JSON] [BlockSpanV2]

-- | V2 BlockSpan data type that always includes nbdr and hashrate
data BlockSpanV2 = BlockSpanV2
  { startBlock :: BlockHeader
  , endBlock :: BlockHeader
  , nbdr :: Double
  , hashrate :: Natural Integer
  }
  deriving (Show, Generic, Typeable)

instance ToJSON   BlockSpanV2
instance FromJSON BlockSpanV2
instance ToSchema BlockSpanV2 where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "BlockSpanV2 schema - always includes nbdr and hashrate"
    & mapped.schema.example ?~ toJSON defaultBlockSpanV2

defaultBlockSpanV2 :: BlockSpanV2
defaultBlockSpanV2 = BlockSpanV2
  { startBlock = defaultBlockHeader
  , endBlock = defaultBlockHeader
  , nbdr = 100.0
  , hashrate = 1000000000
  }


