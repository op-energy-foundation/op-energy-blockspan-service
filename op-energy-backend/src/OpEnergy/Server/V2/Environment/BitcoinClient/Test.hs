{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
module OpEnergy.Server.V2.Environment.BitcoinClient.Test
  ( State(..)
  , init
  , generateBlockChain
  , genesisMediantime
  ) where

import           Control.Concurrent.STM.TVar(TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Concurrent.STM as STM
import           Control.Monad(foldM, void)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Except
                   ( runExceptT, ExceptT(..), throwE)
import           Data.Vector(Vector, (!?))
import qualified Data.Vector as V
import           Prelude hiding (init)
import           Data.Word(Word32)
import           Numeric
import           Flow

import qualified Test.QuickCheck as QC

-- import qualified System.Random.Stateful as Random

import qualified Data.Bitcoin.API as API
import qualified Data.Bitcoin.BlockStats as BlockStats
import qualified Data.Bitcoin.BlockInfo as BlockInfo
import           Data.OpEnergy.API.V1.Natural
import qualified Data.OpEnergy.API.V1.Block as Block
import qualified Data.OpEnergy.API.V1.Hash as Hash

import           OpEnergy.Server.Common(catchBreakT, breakT, exceptTMaybeT)
import           OpEnergy.Server.V2.Core.Call
import qualified OpEnergy.Server.V2.Environment.BitcoinClient.Class as Class
import qualified OpEnergy.Server.V2.Environment.BitcoinClient.Request as Request
import qualified OpEnergy.Server.V2.Environment.Request as Env
import qualified OpEnergy.Server.V2.Environment.Time.Class as Time

data State = State
  { blockChain :: Vector Block.BlockHeader
  , mtipBlockV :: TVar (Maybe Int)
  , time :: Time.TimeM IO
  }

generateBlockChain
  :: Word32
  -> Natural Int
  -> Natural Int
  -> IO (Vector Block.BlockHeader)
generateBlockChain _startTime startHeight endHeight
  | endHeight <= startHeight = return V.empty
generateBlockChain startTime startHeight endHeight = do
  initialBlock <-
    if startHeight == 0
      then return genesisBlock
      else do
        block <- QC.generate QC.arbitrary
        medianTimeOffset <- QC.generate <! QC.choose (0,200)
        return <! block
          { Block.blockHeaderHeight = startHeight
          , Block.blockHeaderTimestamp = startTime
          , Block.blockHeaderMediantime = startTime + medianTimeOffset
          }
  V.iterateNM (fromNatural sz + 1) nextBlock initialBlock
  where
  sz = endHeight - startHeight
  genesisBlock = Block.BlockHeader
    { Block.blockHeaderHash = Hash.verifyHash "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f"
    , Block.blockHeaderPreviousblockhash = Nothing
    , Block.blockHeaderHeight = 0
    , Block.blockHeaderVersion = 1
    , Block.blockHeaderTimestamp = 1231006505
    , Block.blockHeaderBits = 0x1d00ffff
    , Block.blockHeaderNonce = 2083236893
    , Block.blockHeaderDifficulty = 1
    , Block.blockHeaderMerkle_root = Hash.verifyHash "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b"
    , Block.blockHeaderTx_count = 1
    , Block.blockHeaderSize = 285
    , Block.blockHeaderWeight = 1
    , Block.blockHeaderChainwork = verifyNatural $ fst $ head $ readHex "0000000000000000000000000000000000000000000000000000000100010001"
    , Block.blockHeaderMediantime = 1231006505
    , Block.blockHeaderReward = 5000000000
    , Block.blockHeaderChainreward = 5000000000
    }
  nextBlock prevBlock = do
    block <- QC.generate QC.arbitrary
    timeOffset <- QC.generate <! QC.choose (300,900)
    medianTimeOffset <- QC.generate <! QC.choose (300,900)
    return <! block
      { Block.blockHeaderHeight = Block.blockHeaderHeight prevBlock + 1
      , Block.blockHeaderPreviousblockhash = Just <! Block.blockHeaderHash prevBlock
      , Block.blockHeaderTimestamp = Block.blockHeaderTimestamp prevBlock
          + timeOffset
      , Block.blockHeaderMediantime = Block.blockHeaderMediantime prevBlock
          + medianTimeOffset
      }




genesisMediantime :: Word32
genesisMediantime = 1231006505

init
  :: Vector Block.BlockHeader
  -> Time.TimeM IO
  -> (Env.Request-> IO ())
  -> IO (State, Class.BitcoinClient IO)
init blockChain time logAction = do
  mtipIdx <- TVar.newTVarIO Nothing

  let
      state = State
        { blockChain = blockChain
        , mtipBlockV = mtipIdx
        , time = time
        }
  return
    ( state
    , Class.BitcoinClient
      { Class.getBlockchainInfo = do
        logAction1
          ( Env.BitcoinClient <. Request.GetBlockchainInfo <. Call0 <. Just)
          ( getBlockchainInfo state )

      , Class.getBlockStats = \arg-> do
        logAction1
          ( Env.BitcoinClient <. Request.GetBlockStats <. Call (Just arg) <. Just)
          ( getBlockStats state arg)

      , Class.getBlock = \arg-> do
        logAction1
          ( Env.BitcoinClient <. Request.GetBlock <. Call (Just arg) <. Just)
          ( getBlock state arg)

      , Class.getBlockHash = \arg-> do
        logAction1
          ( Env.BitcoinClient <. Request.GetBlockHash <. Call (Just arg) <. Just)
          ( getBlockHash state arg)
      }
    )
  where
    logAction1
      :: (r-> Env.Request)
      -> IO r
      -> IO r
    logAction1 toReq foo = do
      ret <- foo
      logAction <! toReq ret
      return ret

adjustAndGetCurrentTipByCurrentTime
  :: State
  -> IO (Either Failure Block.BlockHeader)
adjustAndGetCurrentTipByCurrentTime state = runExceptT <! do
  mtipBlockIdx <- lift <! TVar.readTVarIO (mtipBlockV state)
  let
      blockchain = blockChain state
      startIdx = maybe 0 (+1) mtipBlockIdx
      mtipIdxAndBlock = do
        idx <- mtipBlockIdx
        block <- blockchain !? idx
        return (idx, block)
  posixTime <- ExceptT <! Time.getPOSIXTime (time state)
  let
      currentTime = floor posixTime
  (_, _, mBlockIdxAndBlock) <- catchBreakT <! foldM
    searchForLatestBlockAndBreak
    (blockchain, currentTime, mtipIdxAndBlock)
    [ startIdx .. (V.length (blockChain state) - 1)]
  (newTipIdx, newTip) <- exceptTMaybeT (Internal "tip not exist yet2")
    <! return mBlockIdxAndBlock
  lift <! STM.atomically <! TVar.writeTVar (mtipBlockV state) (Just newTipIdx)
  return newTip
  where
  searchForLatestBlockAndBreak
    :: Monad m
    => (Vector Block.BlockHeader, Word32, Maybe (Int, Block.BlockHeader))
    -> Int
    -> ExceptT
       (Either Failure (Vector Block.BlockHeader, Word32, Maybe (Int, Block.BlockHeader)))
       m
       (Vector Block.BlockHeader, Word32, Maybe (Int, Block.BlockHeader))
  searchForLatestBlockAndBreak
      acc@(blockchain, currentTime, mpreviousMatchingBlockIdx)
      idx = do
    let
        mcurrentBlock = blockchain !? idx
    currentBlock <- case mcurrentBlock of
      Nothing -> breakT acc
      Just some -> return some
    case ( Block.blockHeaderTimestamp currentBlock > currentTime
         , mpreviousMatchingBlockIdx
         ) of
      ( True, Nothing) -> throwE <! Left <! Internal "no tip exist yet"
      ( True, ret@(Just _)) -> breakT (blockchain, currentTime, ret)
      ( False, _ ) -> return (blockchain, currentTime, Just (idx, currentBlock))

getBlockchainInfo
  :: State
  -> IO (Either Failure API.BlockchainInfo)
getBlockchainInfo state = runExceptT $ do
  tipBlock <- ExceptT <! adjustAndGetCurrentTipByCurrentTime state
  return <! API.BlockchainInfo
    { API.chain = "main"
    , API.blocks = Block.blockHeaderHeight tipBlock
    , API.headers = Block.blockHeaderHeight tipBlock
    , API.bestblockhash = Block.blockHeaderHash tipBlock
    , API.difficulty = Block.blockHeaderDifficulty tipBlock
    , API.time = Just <! Block.blockHeaderTimestamp tipBlock
    , API.mediantime = Block.blockHeaderMediantime tipBlock
    , API.verificationprogress = 1.0
    , API.initialblockdownload = False
    , API.chainwork = Block.blockHeaderChainwork tipBlock
    , API.size_on_disk = 0
    , API.pruned = False
    , API.pruneheight = Nothing
    , API.automatic_pruning = Nothing
    , API.prune_target_size = Nothing
    , API.warnings = ""
    }

--{
--  "chain": "main",
--  "blocks": 939614,
--  "headers": 939614,
--  "bestblockhash": "00000000000000000001163d248961e039af7d745fe71a86589851b912defb92",
--  "difficulty": 145042165424853.3,
--  "time": 1772820398,
--  "mediantime": 1772818189,
--  "verificationprogress": 0.9999998838844727,
--  "initialblockdownload": false,
--  "chainwork": "00000000000000000000000000000000000000011513b882905544d9be46f1a3",
--  "size_on_disk": 825799596570,
--  "pruned": false,
--  "warnings": ""
--}


getBlockHash
  :: State
  -> Block.BlockHeight
  -> IO (Either Failure Block.BlockHash)
getBlockHash state height = runExceptT <! do
  foundBlock <- ExceptT <! getBlockHeaderByHeight state height
  return <! Block.blockHeaderHash foundBlock

getBlockStats
  :: State
  -> Block.BlockHeight
  -> IO (Either Failure BlockStats.BlockStats)
getBlockStats state height = runExceptT <! do
  foundBlock <- ExceptT <! getBlockHeaderByHeight state height
  lift
    ( BlockStats.BlockStats
    <$> QC.generate QC.arbitrary -- { avgfee :: Word64 -- ^ ./src/amount.h:typedef int64_t CAmount
    <*> QC.generate QC.arbitrary -- , avgfeerate :: Word64
    <*> QC.generate QC.arbitrary -- , avgtxsize :: Word64
    <*> pure ( Block.blockHeaderHash foundBlock) -- , blockhash :: BlockHash
    <*> QC.generate QC.arbitrary -- , feerate_percentiles :: [Word64]
    <*> pure height -- , height :: BlockHeight
    <*> QC.generate QC.arbitrary -- , ins :: Word64
    <*> QC.generate QC.arbitrary -- , maxfee :: Word64
    <*> QC.generate QC.arbitrary -- , maxfeerate :: Word64
    <*> QC.generate QC.arbitrary -- , maxtxsize :: Word64
    <*> QC.generate QC.arbitrary -- , medianfee :: Word64
    <*> QC.generate QC.arbitrary -- , mediantxsize :: Word64
    <*> QC.generate QC.arbitrary -- , minfee :: Word64
    <*> QC.generate QC.arbitrary -- , minfeerate :: Word64
    <*> QC.generate QC.arbitrary -- , mintxsize :: Word64
    <*> QC.generate QC.arbitrary -- , outs :: Word64
    <*> QC.generate QC.arbitrary -- , subsidy :: Word64
    <*> QC.generate QC.arbitrary -- , swtotal_size :: Word64
    <*> QC.generate QC.arbitrary -- , swtotal_weight :: Word64
    <*> QC.generate QC.arbitrary -- , swtxs :: Word64 -- ^ ./src/rpc/blockchain.cpp:    int64_t swtxs
    <*> QC.generate QC.arbitrary -- , total_out :: Word64
    <*> QC.generate QC.arbitrary -- , total_size :: Word64
    <*> QC.generate QC.arbitrary -- , total_weight :: Word64
    <*> QC.generate QC.arbitrary -- , totalfee :: Word64
    <*> QC.generate QC.arbitrary -- , utxo_increase :: Int64
    <*> QC.generate QC.arbitrary -- , utxo_size_inc :: Int64
    )

getBlockHeaderByHash
  :: State
  -> Block.BlockHash
  -> IO (Either Failure Block.BlockHeader)
getBlockHeaderByHash state hash = runExceptT <! do
  void <! ExceptT <! adjustAndGetCurrentTipByCurrentTime state
  mfoundBlock <- catchBreakT <! foldM
    (\acc block->
      if Block.blockHeaderHash block == hash
        then breakT <! Just block
        else return acc
    )
    Nothing
    <! blockChain state
  exceptTMaybeT (BadRequest "block with given hash not found")
    <! return mfoundBlock

getBlockHeaderByHeight
  :: State
  -> Block.BlockHeight
  -> IO (Either Failure Block.BlockHeader)
getBlockHeaderByHeight state heightUnchecked = runExceptT <! do
  tipBlock <- ExceptT <! adjustAndGetCurrentTipByCurrentTime state
  height <-
    if Block.blockHeaderHeight tipBlock < heightUnchecked
    then throwE (BadRequest "requested block height is not exist")
    else return heightUnchecked
  mfoundBlock <- catchBreakT <! foldM
    (\acc block->
      if Block.blockHeaderHeight block == height
        then breakT <! Just block
        else return acc
    )
    Nothing
    <! blockChain state
  exceptTMaybeT (BadRequest "block with given hash not found")
    <! return mfoundBlock

getBlock
  :: State
  -> Block.BlockHash
  -> IO (Either Failure BlockInfo.BlockInfo)
getBlock state hash = runExceptT <! do
  foundBlock <- ExceptT <! getBlockHeaderByHash state hash
  lift <! BlockInfo.BlockInfo
    <$> pure (Block.blockHeaderHash foundBlock) -- hash :: BlockHash
    <*> QC.generate QC.arbitrary -- confirmations :: Natural Int
    <*> pure (Block.blockHeaderHeight foundBlock) -- height :: BlockHeight
    <*> QC.generate QC.arbitrary -- version :: Int32
    <*> QC.generate QC.arbitrary -- merkleroot :: Hash
    <*> pure (Block.blockHeaderTimestamp foundBlock) -- time :: Word32
    <*> pure (Block.blockHeaderMediantime foundBlock) -- mediantime :: Word32
    <*> QC.generate QC.arbitrary -- nonce :: Word32
    <*> QC.generate QC.arbitrary -- bits :: Bits
    <*> QC.generate QC.arbitrary -- difficulty :: Double
    <*> QC.generate QC.arbitrary -- chainwork :: Natural Integer
    <*> QC.generate QC.arbitrary -- nTx :: Word64
    <*> pure (Block.blockHeaderPreviousblockhash foundBlock) -- previousblockhash :: Maybe BlockHash
    <*> QC.generate QC.arbitrary -- nextblockhash :: Maybe BlockHash
    <*> QC.generate QC.arbitrary -- strippedsize :: Word64
    <*> QC.generate QC.arbitrary -- size :: Word64
    <*> QC.generate QC.arbitrary -- weight :: Word64

