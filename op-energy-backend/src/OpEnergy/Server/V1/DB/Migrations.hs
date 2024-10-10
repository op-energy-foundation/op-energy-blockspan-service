{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls  #-}
module OpEnergy.Server.V1.DB.Migrations
  where

import           Database.Persist.TH
import           Database.Persist.Sql

import           Data.OpEnergy.API.V1.Natural(Natural)

share [mkPersist sqlSettings, mkMigrate "migrateBlockHeaderDB"] [persistLowerCase|

-- | this table will contain only one record, which contain version of DB, which will be
-- updated at each custom migration
BlockHeaderDB
  version (Natural Int)
|]

