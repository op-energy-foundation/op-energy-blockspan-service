module OpEnergy.Server.V2.Core.HandleRequest.Ping
  ( handle
  ) where

import           OpEnergy.Server.V2.Core.App(AppM)
import           OpEnergy.Server.V2.Core.Call( Failure(..))


handle
  :: Monad m
  => AppM transactionROM transactionM m (Either Failure ())
handle = do
  -- logInput "" -- we can log, but it is expected to always test for a ping
  --                so it will spam the log with pings. That is why we allow
  --                ping to not to log anything: check for a profiling instead
  return (Right ())
