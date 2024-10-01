{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DerivingStrategies         #-}

module OpEnergy.Server.V1.DB.Migrations.BlockHeaderAddChainreward
  ( createInitialBlockHeaderChainreward
  , fillBlockHeaderChainreward
  , finalizeFilledBlockHeaderChainreward
  )where

import           Control.Monad.Trans.Reader(ReaderT)
import           Control.Monad.Trans.Except(runExceptT)
import           Control.Monad.Logger    ( NoLoggingT )
import qualified Data.Text as Text
import           Control.Monad.Trans

import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Pagination
import           Data.OpEnergy.API.V1.Positive( fromPositive)
import           Control.Monad.Trans.Resource(ResourceT)
import           Data.Conduit( (.|) )
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C

import           OpEnergy.Server.Common
import           OpEnergy.Server.V1.Config
import           OpEnergy.Server.V1.DB.Migrations.BlockHeaderAddChainreward.Model

createInitialBlockHeaderChainreward
  :: Config
  -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
createInitialBlockHeaderChainreward _config = do
  rawExecute (Text.unlines $
    [ "ALTER TABLE \"block_header\" ADD COLUMN \"chainreward\" INT8 NULL;" -- we need it to be optional first
    ]) []

finalizeFilledBlockHeaderChainreward
  :: Config
  -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
finalizeFilledBlockHeaderChainreward _config = do
  rawExecute (Text.unlines $
    [ "ALTER TABLE \"block_header\" ALTER COLUMN \"chainreward\" SET NOT NULL;" -- now it should not be NULL
    ]) []

fillBlockHeaderChainreward
  :: Config
  -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
fillBlockHeaderChainreward config = do
  -- 2. fill up new results table
  let isChainRewardNotYetCalculated =
        BlockHeaderChainreward ==. Nothing
  C.runConduit
    $ streamEntities
      [ isChainRewardNotYetCalculated ]
      BlockHeaderId
      (PageSize (fromPositive recordsPerReply))
      Ascend
      (Range Nothing Nothing)
    .| ( C.awaitForever $ \(Entity blockHeaderId blockHeader) -> do
       let previousBlockHeaderNotExist = blockHeaderHeight blockHeader < 1
       if previousBlockHeaderNotExist
         then do
           lift $ update blockHeaderId
             [ BlockHeaderChainreward =. Just (blockHeaderReward blockHeader)
             ]
         else do
           echainReward <- runExceptT $ do
             let
                 previousBlockHeight = blockHeaderHeight blockHeader - 1
             Entity _ previousBlockHeader <- exceptTMaybeT ("failed to find block with height " <> Text.pack (show previousBlockHeight))
               $ lift $ selectFirst [ BlockHeaderHeight ==. previousBlockHeight ] []
             prevBlockChainreward <- exceptTMaybeT ("block with height "
                                                  <> (Text.pack (show previousBlockHeight))
                                                  <> " has no chainreward")
               $ return (blockHeaderChainreward previousBlockHeader)
             return (blockHeaderReward blockHeader + prevBlockChainreward)
           case echainReward of
             Right chainReward -> do
               lift $ update blockHeaderId
                 [ BlockHeaderChainreward =. Just chainReward
                 ]
             Left reason -> do
               let err = "fillBlockHeaderChainreward: " <> reason
                       <> ", which is not expected, crashing the transaction"
               error (Text.unpack err)
       )
    .| C.sinkNull
  where
    recordsPerReply = configRecordsPerReply config

