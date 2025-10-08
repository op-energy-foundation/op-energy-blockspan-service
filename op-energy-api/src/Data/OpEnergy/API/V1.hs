{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.API.V1 where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.Proxy
import           Servant.API
import           Servant.API.WebSocket (WebSocket)
import           Servant.Swagger(HasSwagger(..))
import           Data.Text                  (Text)

import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.API.V1.Block

-- | API specifications of a backend service for Swagger
type V1API
  = "blockspans"
    :> "statistics"
    :> Capture "blockheight" BlockHeight
    :> Capture "spansize" (Positive Int)
    :> Description "Derpecated? Calculates NBDR statistics for a given block height and span. NBDR here is ratio (span * 600 * 100) / (endBlockMedianTime - startBlockMediantime)."
    :> Get '[JSON] Statistics

  :<|> "oe"
    :> "block"
    :> Capture "hash" BlockHash
    :> Description "Returns block's header by a given block hash, including chainwork, that is missing from mempool's blocks' headers cache"
    :> Get '[JSON] BlockHeader

  :<|> "oe"
    :> "blockbyheight"
    :> Capture "height" BlockHeight
    :> Description "Returns block's header by a given block height"
    :> Get '[JSON] BlockHeader

  :<|> "oe"
    :> "blocksbyblockspan"
    :> Capture "startBlockHeight" BlockHeight
    :> Capture "spansize" (Positive Int)
    :> QueryParam "numberOfSpan" (Positive Int)
    :> QueryParam "withNBDR" Bool
    :> QueryParam "withHashrate" Bool
    :> Description "Returns list of blocks' headers by a given block span. In case if withNBDR=true, the answer will contain NBDR value. In case if 'withHashrate=true', the answer will contain Hashrate value. NBDR here is ratio (spansize * 600 * 100) / (endBlockMedianTime - startBlockMediantime). Hashrate here is a ratio (endBlockChainwork - startBlockChainwork) / (endBlockMedianTime - startBlockMediantime). If numberOfSpan is missing, then it will provide blockspans until the current tip."
    :> Get '[JSON] [BlockSpanHeadersNbdrHashrate]

  :<|> "oe"
    :> "blockswithnbdrbyblockspan"
    :> Capture "startBlockHeight" BlockHeight
    :> Capture "spansize" (Positive Int)
    :> QueryParam "numberOfSpan" (Positive Int)
    :> Description "DEPRECATED. use blockbyblockspan instead. Returns list of start and end blocks' headers and their nbdr for each appropriate block span. NBDR here is ratio (spansize * 600 * 100) / (endBlockMedianTime - startBlockMediantime). If numberOfSpan is missing, then it will provide blockspans until the current tip."
    :> Get '[JSON] [BlockSpanHeadersNbdr]

  :<|> "oe"
    :> "blockswithhashratebyblockspan"
    :> Capture "startBlockHeight" BlockHeight
    :> Capture "spansize" (Positive Int)
    :> QueryParam "numberOfSpan" (Positive Int)
    :> Description "Returns list of start and end blocks' headers and their hashrate for each appropriate block span. Hashrate here is a ratio (endBlockChainwork - startBlockChainwork) / (endBlockMedianTime - startBlockMediantime). If numberOfSpan is missing, then it will provide blockspans until the current tip."
    :> Get '[JSON] [BlockSpanHeadersHashrate]

  :<|> "oe"
    :> "blockspanlist"
    :> Capture "startBlockHeight" BlockHeight
    :> Capture "spansize" (Positive Int)
    :> Capture "numberOfSpan" (Positive Int)
    :> Description "Returns list of blockspans started from startBlockHeight of size span and numberOfSpan length "
    :> Get '[JSON] [BlockSpan]

  :<|> "oe"
    :> "git-hash"
    :> Description "returns short hash of commit of the op-energy git repo that had been used to build backend"
    :> Get '[JSON] GitHashResponse


type FakeWSAPI = Get '[JSON] ()
instance HasSwagger WebSocket where
  toSwagger _ = toSwagger api
    where
      api :: Proxy FakeWSAPI
      api = Proxy

data GitHashResponse = GitHashResponse
  { gitCommitHash :: Text
  }
  deriving (Show, Generic, Typeable)
instance ToJSON GitHashResponse
instance FromJSON GitHashResponse
instance ToSchema GitHashResponse where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "GitHashResponse schema"
    & mapped.schema.example ?~ toJSON defaultGitHashResponse
defaultGitHashResponse :: GitHashResponse
defaultGitHashResponse = GitHashResponse
  { gitCommitHash = "12345678"
  }

data BlockSpanHeadersNbdr = BlockSpanHeadersNbdr
  { startBlock :: BlockHeader
  , endBlock :: BlockHeader
  , nbdr :: Double
  }
  deriving (Show, Generic, Typeable)
instance ToJSON   BlockSpanHeadersNbdr
instance FromJSON BlockSpanHeadersNbdr
instance ToSchema BlockSpanHeadersNbdr where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "BlockSpanHeadersNbdr schema"
    & mapped.schema.example ?~ toJSON defaultBlockSpanHeadersNbdr
defaultBlockSpanHeadersNbdr :: BlockSpanHeadersNbdr
defaultBlockSpanHeadersNbdr = BlockSpanHeadersNbdr
  { startBlock = defaultBlockHeader
  , endBlock = defaultBlockHeader
  , nbdr = 100.0
  }

data BlockSpanHeadersNbdrHashrate = BlockSpanHeadersNbdrHashrate
  { startBlock :: BlockHeader
  , endBlock :: BlockHeader
  , nbdr :: Maybe Double
  , hashrate :: Maybe (Natural Integer)
  }
  deriving (Show, Generic, Typeable)
instance ToJSON   BlockSpanHeadersNbdrHashrate
instance FromJSON BlockSpanHeadersNbdrHashrate
instance ToSchema BlockSpanHeadersNbdrHashrate where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "BlockSpanHeadersNbdrHashrate schema"
    & mapped.schema.example ?~ toJSON defaultBlockSpanHeadersNbdrHashrate
defaultBlockSpanHeadersNbdrHashrate :: BlockSpanHeadersNbdrHashrate
defaultBlockSpanHeadersNbdrHashrate = BlockSpanHeadersNbdrHashrate
  { startBlock = defaultBlockHeader
  , endBlock = defaultBlockHeader
  , nbdr = Just 100.0
  , hashrate = Just 1
  }

data BlockSpanHeadersHashrate = BlockSpanHeadersHashrate
  { startBlock :: BlockHeader
  , endBlock :: BlockHeader
  , hashrate :: Natural Integer
  }
  deriving (Show, Generic, Typeable)
instance ToJSON   BlockSpanHeadersHashrate
instance FromJSON BlockSpanHeadersHashrate
instance ToSchema BlockSpanHeadersHashrate where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "BlockSpanHeadersHashrate schema"
    & mapped.schema.example ?~ toJSON defaultBlockSpanHeadersHashrate
defaultBlockSpanHeadersHashrate :: BlockSpanHeadersHashrate
defaultBlockSpanHeadersHashrate = BlockSpanHeadersHashrate
  { startBlock = defaultBlockHeader
  , endBlock = defaultBlockHeader
  , hashrate = 100
  }

data BlockSpanSummary = BlockSpanSummary
  { startBlockHeight :: Natural Int
  , endBlockHeight :: Natural Int
  , nbdr :: Double
  , hashrate :: Natural Integer
  }
  deriving (Show, Generic, Typeable)
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

data NbdrStatistics = NbdrStatistics
  { avg :: Double
  , stddev :: Double
  }
  deriving (Show, Generic, Typeable)

defaultNbdrStatistics :: NbdrStatistics
defaultNbdrStatistics = NbdrStatistics
  { avg = 1
  , stddev = 1
  }

instance ToJSON NbdrStatistics
instance FromJSON NbdrStatistics
instance ToSchema NbdrStatistics where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "NbdrStatistics schema"
    & mapped.schema.example ?~ toJSON defaultNbdrStatistics

data Statistics = Statistics
  { nbdr :: NbdrStatistics
  }
  deriving (Show, Generic, Typeable)

defaultStatistics :: Statistics
defaultStatistics = Statistics
  { nbdr = defaultNbdrStatistics
  }

instance ToJSON Statistics
instance FromJSON Statistics
instance ToSchema Statistics where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "Statistics schema"
    & mapped.schema.example ?~ toJSON defaultStatistics

type NLockTime = Natural Int



