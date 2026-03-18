{-# LANGUAGE Safe #-}
{-# LANGUAGE RankNTypes #-}
module OpEnergy.Server.V2.Environment.Profiler
  ( profile
  , profileReq
  ) where

import           Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Char as Char
import           Control.Monad.Reader( ask, asks)
import           Control.Monad.Trans(lift)
import           Flow

import           Data.Text.Show(tshow)

import           OpEnergy.Server.V2.Core.App
import qualified OpEnergy.Server.V2.Core.Request as CoreRequest
import qualified OpEnergy.Server.V2.Environment.Profiler.Class as Class
import qualified OpEnergy.Server.V2.Environment as Env

profile
  :: Monad m
  => Text
  -> AppM transactionROM transactionM m r
  -> AppM transactionROM transactionM m r
profile header action = do
  env <- ask
  ptr <- asks Env.profiler
  callstack <- asks Env.callstack
  lift $ Class.profile ptr header $ do
    runAppM (callstack <> "." <> header) env action

profileReq
  :: Monad m
  => CoreRequest.Request
  -> AppM transactionROM transactionM m r
  -> AppM transactionROM transactionM m r
profileReq request action = do
  profile header action
  where
  header = tshow request |> Text.takeWhile Char.isAlphaNum


