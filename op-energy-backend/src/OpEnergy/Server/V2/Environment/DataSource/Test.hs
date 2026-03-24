{-# LANGUAGE Safe #-}
{-# LANGUAGE RankNTypes #-}
module OpEnergy.Server.V2.Environment.DataSource.Test
  ( init
  , State(..)
  ) where

import           Prelude hiding (init)
import           Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.Trans.Maybe( MaybeT(..), runMaybeT)
import           Control.Monad.Trans( lift)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Flow

import           Data.OpEnergy.API.V1.Block
                   ( BlockHeight
                   , BlockHeader(..)
                   , BlockHash
                   )
import           OpEnergy.Server.V2.Core.Call
import qualified OpEnergy.Server.V2.Environment.Request as Env
import qualified OpEnergy.Server.V2.Environment.DataSource.Request as Request
import qualified OpEnergy.Server.V2.Environment.DataSource.Class as Class

data State = State
  { blocksV :: TVar (Map BlockHeight BlockHeader)
  , hashesV :: TVar (Map BlockHash BlockHeight)
  }

init
  :: ( Env.Request-> IO ())
  -> ( Env.Request-> STM ())
  -> IO (State, Class.DataSource STM STM IO)
init logAction logActionSTM = do
  blocks <- TVar.newTVarIO Map.empty
  hashes <- TVar.newTVarIO Map.empty
  let
      state = State
        { blocksV = blocks
        , hashesV = hashes
        }
  return
    ( state
    , Class.DataSource
      { Class.withTransactionRO = \foo-> logAction1
        (Env.DataSource <. Request.WithTransactionRO <. Call0 <. Just <. Right)
        <! ( Right <$> STM.atomically foo)

      , Class.withTransaction = \foo-> logAction1
        (Env.DataSource <. Request.WithTransaction <. Call0 <. Just <. Right)
        <! ( Right <$> STM.atomically foo)

      , Class.storeBlockHeader = \arg-> logActionSTM1
        (Env.DataSource <. Request.WriteRequest <. Request.StoreBlockHeader <. Call (Just arg) <. Just <. Right)
        <! storeBlockHeader state arg

      , Class.mgetBlockHeaderByHeightRO = \arg-> logActionSTM1
        ( Env.DataSource <. Request.ReadRequest <. Request.MGetBlockHeaderByHeightRO <. Call (Just arg)
        <. Just <. Right
        )
        <! mgetBlockHeaderByHeightRO state arg

      }
    )
  where
  logAction1 :: (r-> Env.Request)-> IO r-> IO r
  logAction1 toRequest foo = do
    v <- foo
    logAction (toRequest v)
    return v

  logActionSTM1 :: (r-> Env.Request)-> STM r-> STM r
  logActionSTM1 toRequest foo = do
    v <- foo
    logActionSTM (toRequest v)
    return v

storeBlockHeader
  :: State
  -> BlockHeader
  -> STM ()
storeBlockHeader state header = do
  TVar.modifyTVar (blocksV state) <! Map.insert height header
  TVar.modifyTVar (hashesV state) <! Map.insert hash height
  where
  height = blockHeaderHeight header
  hash = blockHeaderHash header

mgetBlockHeaderByHeightRO
  :: State
  -> BlockHeight
  -> STM (Maybe BlockHeader)
mgetBlockHeaderByHeightRO state height = runMaybeT <! do
  blocks <- lift <! TVar.readTVar (blocksV state)
  MaybeT <! pure <! Map.lookup height blocks


