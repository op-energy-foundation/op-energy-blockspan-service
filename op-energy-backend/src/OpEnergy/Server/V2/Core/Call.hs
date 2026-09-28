{-# LANGUAGE Safe #-}
module OpEnergy.Server.V2.Core.Call
  ( Call0(..)
  , Call(..)
  , Cast(..)
  , Failure(..)
  ) where

import           Data.Text(Text)

data Failure
  = Internal Text
  | BadRequest Text
  deriving (Show)

newtype Show r => Call0 r = Call0 (Maybe (Either Failure r))
  deriving (Show)

newtype (Show req) => Cast req = Cast (Maybe req)
  deriving (Show)

data
  (Show request, Show response) =>
  Call request response = Call (Maybe request) (Maybe (Either Failure response))
  deriving (Show)

