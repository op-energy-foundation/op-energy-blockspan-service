module OpEnergy.Server.Common
  ( exceptTMaybeT
  )where

import           Control.Monad.Trans.Except (ExceptT(..))

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

