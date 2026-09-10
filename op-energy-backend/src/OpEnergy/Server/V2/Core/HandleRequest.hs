module OpEnergy.Server.V2.Core.HandleRequest
  ( handleRequest
  ) where

import           Flow

import           Data.Text.Show( tshow)

import           OpEnergy.Server.V2.Core.App(AppM)
import           OpEnergy.Server.V2.Core.Call
import           OpEnergy.Server.V2.Core.Request(Request(..))
import           OpEnergy.Server.V2.Environment.Profiler
import           OpEnergy.Server.V2.Environment.Logger

import qualified OpEnergy.Server.V2.Core.HandleRequest.Ping as Ping
import qualified OpEnergy.Server.V2.Core.HandleRequest.GetBlockHeaderByHeight
                   as GetBlockHeaderByHeight

handleRequest
  :: ( Monad m
     , Monad transactionROM
     )
  => Request
  -> AppM transactionROM transactionM m Request
handleRequest req@(Ping _) = Ping <. Call0 <. Just <$> profileReq req Ping.handle
handleRequest req@(GetBlockByHeight (Call Nothing _)) =
      ( GetBlockByHeight <. Call Nothing <. Just)
  <$> ( profileReq req <! do
        logInput <! tshow (Nothing :: Maybe ())
        return <! Left <! BadRequest "GetBlockByHeight: height expected"
      )
handleRequest req@(GetBlockByHeight (Call (Just height) _)) =
      ( GetBlockByHeight <. Call (Just height) <. Just)
  <$> ( profileReq req <! GetBlockHeaderByHeight.getBlockHeaderByHeight height)
