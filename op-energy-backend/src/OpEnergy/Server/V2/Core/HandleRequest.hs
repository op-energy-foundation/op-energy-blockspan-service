module OpEnergy.Server.V2.Core.HandleRequest
  ( handleRequest
  ) where

import           OpEnergy.Server.V2.Core.App(AppM)
import           OpEnergy.Server.V2.Core.Call(Call0(..))
import           OpEnergy.Server.V2.Core.Request(Request(..))
import           OpEnergy.Server.V2.Environment.Profiler
-- import           OpEnergy.Server.V2.Environment.Logger

import qualified OpEnergy.Server.V2.Core.HandleRequest.Ping as Ping

handleRequest
  :: Monad m
  => Request
  -> AppM transactionROM transactionM m Request
handleRequest req@(Ping _) = Ping . Call0 . Just <$> profileReq req Ping.handle

