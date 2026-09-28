module OpEnergy.Server.V2.Core.HandleRequest.GetBlockHeaderByHeight
  ( getBlockHeaderByHeight
  ) where

import           Control.Monad.Reader(asks)
import           Control.Monad.Trans( lift)
import           Control.Monad.Trans.Except(ExceptT(..))
import           Flow

import           Data.OpEnergy.API.V1.Block(BlockHeight, BlockHeader(..))
import           Data.Text.Show( tshow)

import           OpEnergy.Server.Common
import           OpEnergy.Server.V2.Core.Call
import           OpEnergy.Server.V2.Core.App(AppM)
import           OpEnergy.Server.V2.Environment.Profiler
import           OpEnergy.Server.V2.Environment.Logger
import           OpEnergy.Server.V2.Environment.DataSource
import qualified OpEnergy.Server.V2.Environment.DataSource.Class as DataSource
import qualified OpEnergy.Server.V2.Environment as Env

getBlockHeaderByHeight
  :: ( Monad m
     , Monad transactionROM
     )
  => BlockHeight
  -> AppM transactionROM transactionM m (Either Failure BlockHeader)
getBlockHeaderByHeight height =
    let name = "getBlockHeaderByHeight"
    in profile name <! runExceptPrefixTF name <! do
  lift <! logInput <! tshow height
  dataSource <- asks Env.dataSource
  mblock <- ExceptT <! withTransactionRO
    <! DataSource.mgetBlockHeaderByHeightRO dataSource height
  exceptTMaybeT (BadRequest "block with given height is not exist")
    <! return mblock

