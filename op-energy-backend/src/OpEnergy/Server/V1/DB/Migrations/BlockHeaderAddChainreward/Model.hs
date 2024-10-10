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

module OpEnergy.Server.V1.DB.Migrations.BlockHeaderAddChainreward.Model
  where

import           Data.Word(Word64)

import           Database.Persist.TH
import           Database.Persist
import           Database.Persist.Postgresql

-- | here goes migration-local representations of the tables.
-- the reason why are exist is that we don't know the "actual" schema of the tables as we can
-- be not the last migration. The only thing we know are:
-- 1. previous schema of the BlockHeader table
-- 2. schema of the BlockHeader tables after migration
--
-- we can't use any our custom data type for a fields as we don't know if they stayed the same
-- or had been changed since.
--
-- at the same time, migration-only models are not have to have all the fields
-- for models that require no creation: only those fields, that are used in
-- migrations as the rest of the fields won't be touched during migrations
share [mkPersist sqlSettings] [persistLowerCase|
BlockHeader
  height Int
  reward Word64
  chainreward Word64 Maybe -- sum of BlockHeader.reward for a range [ 0 .. height]
|]

