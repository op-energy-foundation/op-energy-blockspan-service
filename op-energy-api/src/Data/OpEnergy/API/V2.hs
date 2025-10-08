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

import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.API.V1.Block
import qualified Data.OpEnergy.API.V1 as V1
import           Data.OpEnergy.API.Tags

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

  :<|> "blockspanlist"
    :> Capture "startBlockHeight" BlockHeight
    :> Capture "spansize" (Positive Int)
    :> Capture "numberOfSpan" (Positive Int)
    :> Description "Returns list of blockspans started from startBlockHeight of size span and numberOfSpan length "
    :> Get '[JSON] [BlockSpan]

  :<|> "blockspan"
    :> Capture "blockHeight" BlockHeight
    :> QueryParam "spanSize" (Positive Int)
    :> Description "Returns a single blockspan ending at the specified block height. A blockspan is the start and end block for a blockspan, along with summary data (NBDR and hashrate) and optional header infos for blocks. spanSize defaults to 24 if not specified."
    :> Get '[JSON] V1.BlockSpanHeadersNbdrHashrate

  :<|> "git-hash"
    :> Description "returns short hash of commit of the op-energy git repo that had been used to build backend"
    :> Get '[JSON] V1.GitHashResponse





