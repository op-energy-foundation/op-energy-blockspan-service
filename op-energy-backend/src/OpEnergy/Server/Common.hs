{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Server.Common
  ( exceptTMaybeT
  , eitherLogThrowOrReturn
  , runExceptPrefixT
  , eitherException
  )where

import           Data.Text(Text)
import qualified Data.Text as Text

import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Logger(logError)
import           Control.Exception.Safe (SomeException)
import qualified Control.Exception.Safe as E

import           OpEnergy.Server.V1.Class ( AppM, runLogging)
import           Data.OpEnergy.API.V1.Error (throwJSON)
import           Servant.Server.Internal.ServerError(err500)

exceptTMaybeT
  :: Monad m
  => l
  -> m (Maybe r)
  -> ExceptT l m r
exceptTMaybeT onNothing action = ExceptT $ do
  mret <- action
  case mret of
    Nothing -> return (Left onNothing)
    Just result -> return (Right result)

-- | In case of success this function will just return @r@
-- In case of failure, it will log error reason and throw it as a JSON error.
eitherLogThrowOrReturn
  :: AppM (Either Text r)
  -> AppM r
eitherLogThrowOrReturn foo = do
  r <- foo
  case r of
    Left reason-> do
      runLogging $ $(logError) reason
      throwJSON err500 reason
    Right ret -> return ret

-- | this function is basically an extension to @runExceptT@ with the addition
-- of @prefix@ to the error reason in case of failure
runExceptPrefixT
  :: (Monad m)
  => Text
  -> ExceptT Text m r
  -> m (Either Text r)
runExceptPrefixT prefix payload = do
  ret <- runExceptT payload
  return $! either (\reason -> Left (prefix <> ": " <> reason)) Right ret

--
-- | this functions's goal is to handle possible exception into @Either@ type
-- in order to wrap side-effectful routine into ExceptT transformer
-- Example:
-- @ eitherException $ readFile "/file/not/found" @
eitherException
  :: ( Monad m
     , E.MonadCatch m
     )
  => m r
  -> m (Either Text r)
eitherException next = do
  !ret <- E.handle
    (\(e::SomeException)->
      return (Left (Text.pack (show e)))
    )
    (do
      !ret <- next
      return (Right ret)
    )
  return ret

