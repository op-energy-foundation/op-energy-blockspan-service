module OpEnergy.Server.V2.Core.HandleRequest.GetBlockHeaderByHash
  ( getBlockHeaderByHash
  ) where

import           Control.Monad.Reader(asks)
import           Control.Monad.Trans( lift)
import           Control.Monad.Trans.Except(ExceptT(..))
import           Flow

import           Data.OpEnergy.API.V1.Block(BlockHash, BlockHeader(..))
import           Data.Text.Show( tshow)

import           OpEnergy.Server.Common
import           OpEnergy.Server.V2.Core.Call
import           OpEnergy.Server.V2.Core.App(AppM)
import           OpEnergy.Server.V2.Environment.Profiler
import           OpEnergy.Server.V2.Environment.Logger
import           OpEnergy.Server.V2.Environment.DataSource
import qualified OpEnergy.Server.V2.Environment.DataSource.Class as DataSource
import qualified OpEnergy.Server.V2.Environment as Env

getBlockHeaderByHash
  :: ( Monad m
     , Monad transactionROM
     )
  => BlockHash
  -> AppM transactionROM transactionM m (Either Failure BlockHeader)
getBlockHeaderByHash hash =
    let name = "getBlockHeaderByHash"
    in profile name <! runExceptPrefixTF name <! do
  lift <! logInput <! tshow hash
  dataSource <- asks Env.dataSource
  mblock <- ExceptT <! withTransactionRO
    <! DataSource.mgetBlockHeaderByHashRO dataSource hash
  exceptTMaybeT (BadRequest "block with given hash is not exist")
    <! return mblock

