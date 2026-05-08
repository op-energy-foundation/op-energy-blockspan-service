{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
module OpEnergy.Server.V2.Environment.BitcoinClient.Test
  ( State(..)
  , init
  , generateBlockChain
  , genesisMediantime
  , addReorganizations
  , getBlockHeaderByHash
  , adjustAndGetCurrentTipByCurrentTime
  ) where

import           Control.Concurrent.STM.TVar(TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Concurrent.STM as STM
import           Control.Monad(foldM, void, when, foldM)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Except
                   ( ExceptT(..), throwE, withExceptT, runExceptT)
import           Data.Vector(Vector, (!?))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.List as List
import           Prelude hiding (init)
import           Data.Word(Word32)
import           Data.Text.Show(tshow)
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

import           OpEnergy.Server.Common
                   ( catchBreakT, breakT, exceptTMaybeT, runExceptPrefixTF)
import           OpEnergy.Server.V2.Core.Call
import qualified OpEnergy.Server.V2.Environment.BitcoinClient.Class as Class
import qualified OpEnergy.Server.V2.Environment.BitcoinClient.Request as Request
import qualified OpEnergy.Server.V2.Environment.Request as Env
import qualified OpEnergy.Server.V2.Environment.Time.Class as Time
import qualified OpEnergy.Server.V2.Environment.Profiler.Class as Profiler

data State = State
  { blockChain :: Vector Block.BlockHeader
  , mtipBlockV :: TVar (Maybe Int, Map Block.BlockHash Int)
  , time :: Time.TimeM IO
  , profiler :: Profiler.Profiler IO
  , callstack :: Text
  }

-- | generates new block chain of random blocks. Block heights are sequental
-- and each next block refers to a hash of the previous block. Mediantime and
-- timestamp of each next block is greater than appropriate values of the
-- current block
generateBlockChain
  :: Word32
  -> Natural Int
  -> Natural Int
  -> IO (Vector Block.BlockHeader)
generateBlockChain startTime startHeight endHeight = do
  initialBlock <-
    if startHeight == 0
      then return <! genesisBlock
        { Block.blockHeaderTimestamp = startTime
        , Block.blockHeaderMediantime = startTime
        }
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
  :: Text
  -> Vector Block.BlockHeader
  -> Time.TimeM IO
  -> (Env.Request-> IO ())
  -> Profiler.Profiler IO
  -> IO (State, Class.BitcoinClient IO)
init context blockChain time logAction profiler = do
  mtipIdx <- TVar.newTVarIO (Nothing, Map.empty)
  let
      state = State
        { blockChain = blockChain
        , mtipBlockV = mtipIdx
        , time = time
        , profiler = profiler
        , callstack = context
        }
  return
    ( state
    , Class.BitcoinClient
      { Class.getBlockchainInfo =
        logAction1
          ( Env.BitcoinClient <. Request.GetBlockchainInfo <. Call0 <. Just)
          ( runExceptPrefixTF "getBlockchainInfo" <! do
            void <! ExceptT <! adjustAndGetCurrentTipByCurrentTime state
            ExceptT <! getBlockchainInfo state
          )

      , Class.getBlockStats = \arg->
        logAction1
          ( Env.BitcoinClient <. Request.GetBlockStats <. Call (Just arg) <. Just)
          <! runExceptPrefixTF "getBlockStats" <! do
            void <! ExceptT <! adjustAndGetCurrentTipByCurrentTime state
            ExceptT <! getBlockStats state arg

      , Class.getBlock = \arg->
        logAction1
          ( Env.BitcoinClient <. Request.GetBlock <. Call (Just arg) <. Just)
          <! runExceptPrefixTF "getBlock" <! do
            void <! ExceptT <! adjustAndGetCurrentTipByCurrentTime state
            ExceptT <! getBlock state arg

      , Class.getBlockHash = \arg->
        logAction1
          ( Env.BitcoinClient <. Request.GetBlockHash <. Call (Just arg) <. Just)
          <! runExceptPrefixTF "getBlockHash" <! do
            void <! ExceptT <! adjustAndGetCurrentTipByCurrentTime state
            ExceptT <! getBlockHash state arg

      , Class.getBlockByHash = \arg->
        logAction1
          ( Env.BitcoinClient <. Request.GetBlockByHash <. Call (Just arg) <. Just)
          <! runExceptPrefixTF "getBlock" <! do
            void <! ExceptT <! adjustAndGetCurrentTipByCurrentTime state
            ExceptT <! getBlockHeaderByHash state arg

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


foldMI
  :: Monad m
  => (acc -> Int-> m acc)
  -> acc
  -> Int
  -> m acc
foldMI foo !acc !n
  | n < 1 = foo acc 0
foldMI foo !acc !n = do
  newAcc <- foo acc n
  foldMI foo newAcc newN
  where
  newN = n - 1

adjustAndGetCurrentTipByCurrentTime
  :: State
  -> IO (Either Failure Block.BlockHeader)
adjustAndGetCurrentTipByCurrentTime state0 =
    let name = "adjustAndGetCurrentTipByCurrentTime"
        state = state0
          { callstack = (callstack state0) <> "." <> name
          }
        newCallstack = callstack state
    in Profiler.profile profilerI newCallstack <! runExceptPrefixTF name <! do
  (mtipBlockIdx, hashIdx) <- lift <! TVar.readTVarIO (mtipBlockV state)
  let
      blockchain = blockChain state
      startIdx = maybe 0 (+1) mtipBlockIdx
      lastIdx = V.length blockchain - 1
      mtipIdxAndBlock = do
        idx <- mtipBlockIdx
        block <- blockchain !? idx
        return (idx, block)
  posixTime <- ExceptT <! Time.getPOSIXTime (time state)
  let
      currentTime = floor posixTime
      foldMCallstack = newCallstack <> ".foldM"
  ( _, _, mBlockIdxAndBlock, newHashIdx) <- ExceptT
    <! Profiler.profile profilerI foldMCallstack <! runExceptT
    <! catchBreakT <! foldM
      searchForLatestBlockAndBreak
      (blockchain, currentTime, mtipIdxAndBlock, hashIdx)
      [ startIdx .. lastIdx]
  (newTipIdx, newTip) <- exceptTMaybeT (Internal "tip not exist yet2")
    <! return mBlockIdxAndBlock
  lift <! STM.atomically
    <! TVar.writeTVar (mtipBlockV state) (Just newTipIdx, newHashIdx)
  return newTip
  where
  profilerI = profiler state0
  searchForLatestBlockAndBreak
    :: Monad m
    => ( Vector Block.BlockHeader
       , Word32
       , Maybe (Int, Block.BlockHeader)
       , Map Block.BlockHash Int
       )
    -> Int
    -> ExceptT
       ( Either
         Failure
         ( Vector Block.BlockHeader
         , Word32
         , Maybe (Int, Block.BlockHeader)
         , Map Block.BlockHash Int
         )
       )
       m
       ( Vector Block.BlockHeader
       , Word32
       , Maybe (Int, Block.BlockHeader)
       , Map Block.BlockHash Int
       )
  searchForLatestBlockAndBreak
      acc@(!blockchain, !currentTime, !mpreviousMatchingBlockIdx, !hashIdx)
      idx = do
    let
        mcurrentBlock = blockchain !? idx
    currentBlock <- case mcurrentBlock of
      Nothing -> breakT acc
      Just some -> return some
    let
        newHashIdx = Map.insert (Block.blockHeaderHash currentBlock) idx hashIdx
    case ( Block.blockHeaderTimestamp currentBlock > currentTime
         , mpreviousMatchingBlockIdx
         ) of
      ( True, Nothing) -> throwE <! Left <! Internal <! Text.unlines
        [ "no tip exist yet: "
        , "idx: " <> tshow idx
        , "block: " <> tshow (Block.blockHeaderHeight currentBlock)
        , "block time stamp: " <> tshow (Block.blockHeaderTimestamp currentBlock)
        , ", currentTime: " <> tshow currentTime
        ]
      ( True, ret@(Just _)) -> breakT (blockchain, currentTime, ret, newHashIdx)
      ( False, _ ) -> return ( blockchain
                             , currentTime
                             , Just (idx, currentBlock)
                             , newHashIdx
                             )

getBlockchainInfo
  :: State
  -> IO (Either Failure API.BlockchainInfo)
getBlockchainInfo state0 =
    let
        name = "getBlockchainInfo"
        state = state0
          { callstack = (callstack state0) <> "." <> name
          }
        newCallstack = callstack state
        profilerI = profiler state
        blockchain = blockChain state
    in Profiler.profile profilerI newCallstack <! runExceptPrefixTF name <! do
  (mtipBlockIdx, _) <- lift <! TVar.readTVarIO (mtipBlockV state)
  tipBlockIdx <- exceptTMaybeT (Internal "no tip discovered yet")
    <! return mtipBlockIdx
  let
      mtipBlock = blockchain !? tipBlockIdx
  tipBlock <- exceptTMaybeT (Internal "tip block is missing, unexpected")
    <! return mtipBlock
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
getBlockHash state0 height =
    let
      name = "getBlockHash"
      newCallstack = (callstack state0) <> "." <> name
      state = state0
        { callstack = newCallstack
        }
      profilerI = profiler state0
    in Profiler.profile profilerI newCallstack <! runExceptPrefixTF name <! do
  foundBlock <- ExceptT <! getBlockHeaderByHeight state height
  return <! Block.blockHeaderHash foundBlock

getBlockStats
  :: State
  -> Block.BlockHeight
  -> IO (Either Failure BlockStats.BlockStats)
getBlockStats state0 height =
    let
        name = "getBlockStats"
        state = state0
          { callstack = (callstack state0) <> "." <> name
          }
        profilerI = profiler state0
        newCallstack = callstack state
    in Profiler.profile profilerI newCallstack <! runExceptPrefixTF name <! do
  foundBlock <- ExceptT <! getBlockHeaderByHeight state height
  lift
    ( BlockStats.BlockStats
    <$> return 123 -- QC.generate QC.arbitrary -- { avgfee :: Word64 -- ^ ./src/amount.h:typedef int64_t CAmount
    <*> return 123 -- QC.generate QC.arbitrary -- , avgfeerate :: Word64
    <*> return 123 -- QC.generate QC.arbitrary -- , avgtxsize :: Word64
    <*> pure ( Block.blockHeaderHash foundBlock) -- , blockhash :: BlockHash
    <*> return [123] -- QC.generate QC.arbitrary -- , feerate_percentiles :: [Word64]
    <*> pure height -- , height :: BlockHeight
    <*> return 123 -- QC.generate QC.arbitrary -- , ins :: Word64
    <*> return 123 -- QC.generate QC.arbitrary -- , maxfee :: Word64
    <*> return 123 -- QC.generate QC.arbitrary -- , maxfeerate :: Word64
    <*> return 123 -- QC.generate QC.arbitrary -- , maxtxsize :: Word64
    <*> return 123 -- QC.generate QC.arbitrary -- , medianfee :: Word64
    <*> return 123 -- QC.generate QC.arbitrary -- , mediantxsize :: Word64
    <*> return 123 -- QC.generate QC.arbitrary -- , minfee :: Word64
    <*> return 123 -- QC.generate QC.arbitrary -- , minfeerate :: Word64
    <*> return 123 -- QC.generate QC.arbitrary -- , mintxsize :: Word64
    <*> return 123 -- QC.generate QC.arbitrary -- , outs :: Word64
    <*> return 123 -- QC.generate QC.arbitrary -- , subsidy :: Word64
    <*> return 123 -- QC.generate QC.arbitrary -- , swtotal_size :: Word64
    <*> return 123 -- QC.generate QC.arbitrary -- , swtotal_weight :: Word64
    <*> return 123 -- QC.generate QC.arbitrary -- , swtxs :: Word64 -- ^ ./src/rpc/blockchain.cpp:    int64_t swtxs
    <*> return 123 -- QC.generate QC.arbitrary -- , total_out :: Word64
    <*> return 123 -- QC.generate QC.arbitrary -- , total_size :: Word64
    <*> return 123 -- QC.generate QC.arbitrary -- , total_weight :: Word64
    <*> return 123 -- QC.generate QC.arbitrary -- , totalfee :: Word64
    <*> return 123 -- QC.generate QC.arbitrary -- , utxo_increase :: Int64
    <*> return 123 -- QC.generate QC.arbitrary -- , utxo_size_inc :: Int64
    )

getBlockHeaderByHash
  :: State
  -> Block.BlockHash
  -> IO (Either Failure Block.BlockHeader)
getBlockHeaderByHash state0 hash =
    let
        name = "getBlockHeaderByHash"
        state = state0
          { callstack = (callstack state0) <> "." <> name
          }
        profilerI = profiler state0
        newCallstack = callstack state
    in Profiler.profile profilerI newCallstack <! runExceptPrefixTF name <! do
  ( _, hashIdx) <- lift <! TVar.readTVarIO (mtipBlockV state)
  idx <- exceptTMaybeT (BadRequest "no block with given hash")
    <! return <! Map.lookup hash hashIdx
  let
      blockchain = blockChain state
  exceptTMaybeT (Internal "hash map has hash, but block chain has no such block")
    <! return <! blockchain !? idx

getBlockHeaderByHeight
  :: State
  -> Block.BlockHeight
  -> IO (Either Failure Block.BlockHeader)
getBlockHeaderByHeight state0 heightUnchecked =
    let
        name = "getBlockHeaderByHeight"
        newCallstack = (callstack state0) <> "." <> name
        state = state0
          { callstack = newCallstack
          }
        profilerI = profiler state0
        blockchain = blockChain state
    in Profiler.profile profilerI newCallstack <! runExceptPrefixTF name <! do
  (mtipBlockIdx, _) <- lift <! TVar.readTVarIO (mtipBlockV state)
  tipBlockIdx <- exceptTMaybeT (Internal "no tip discovered yet")
    <! return mtipBlockIdx
  let
      mtipBlock = blockchain !? tipBlockIdx
  tipBlock <- exceptTMaybeT (Internal "tip block is missing, unexpected")
    <! return mtipBlock
  (mtipBlockIdx, _) <- lift <! TVar.readTVarIO (mtipBlockV state)
  tipBlockIdx <- exceptTMaybeT (Internal "no tip discovered yet")
    <! return mtipBlockIdx
  height <-
    if Block.blockHeaderHeight tipBlock < heightUnchecked
    then throwE (BadRequest "requested block height is not exist")
    else return heightUnchecked
  let
      foldMICallstack = newCallstack <> ".foldMI"
  (mfoundBlock, _, _, _) <- ExceptT
    <! Profiler.profile profilerI foldMICallstack <! runExceptT
    <! catchBreakT <! foldMI
      searchHashWithinBlockchain
      (Nothing, state, height, tipBlock)
      tipBlockIdx
  exceptTMaybeT (BadRequest "block with given height not found")
    <! return mfoundBlock
  where
  searchHashWithinBlockchain (!acc, !state0, !height, !block) _ =
    let
        name = "searchHashWithinBlockchain"
        _profilerI = profiler state0
        !newCallstack = (callstack state0) <> "." <> name
        state = state0
          { callstack = newCallstack
          }
    in do
      if Block.blockHeaderHeight block == height
        then breakT (Just block, state0, height, block)
        else do
          prevBlockHash <- exceptTMaybeT
            (Left <! Internal "block with given hash not found in block chain")
            <! return <! Block.blockHeaderPreviousblockhash block
          nextBlock <- withExceptT Left <! ExceptT
            <! getBlockHeaderByHash state prevBlockHash
          return (acc, state0, height, nextBlock)

getBlock
  :: State
  -> Block.BlockHash
  -> IO (Either Failure BlockInfo.BlockInfo)
getBlock state0 hash =
    let
        name = "getBlock"
        state = state0
          { callstack = (callstack state0) <> "." <> name
          }
        profilerI = profiler state0
        newCallstack = callstack state
    in Profiler.profile profilerI newCallstack <! runExceptPrefixTF name <! do
  foundBlock <- ExceptT <! getBlockHeaderByHash state hash
  lift <! BlockInfo.BlockInfo
    <$> pure (Block.blockHeaderHash foundBlock) -- hash :: BlockHash
    <*> return 123 -- QC.generate QC.arbitrary -- confirmations :: Natural Int
    <*> pure (Block.blockHeaderHeight foundBlock) -- height :: BlockHeight
    <*> pure (Block.blockHeaderVersion foundBlock) -- version :: Int32
    <*> pure (Block.blockHeaderMerkle_root foundBlock) -- merkleroot :: Hash
    <*> pure (Block.blockHeaderTimestamp foundBlock) -- time :: Word32
    <*> pure (Block.blockHeaderMediantime foundBlock) -- mediantime :: Word32
    <*> pure (Block.blockHeaderNonce foundBlock) -- nonce :: Word32
    <*> pure (Block.blockHeaderBits foundBlock) -- bits :: Bits
    <*> pure (Block.blockHeaderDifficulty foundBlock) -- difficulty :: Double
    <*> pure (Block.blockHeaderChainwork foundBlock) -- chainwork :: Natural Integer
    <*> pure (Block.blockHeaderTx_count foundBlock) -- nTx :: Word64
    <*> pure (Block.blockHeaderPreviousblockhash foundBlock) -- previousblockhash :: Maybe BlockHash
    <*> return Nothing -- QC.generate QC.arbitrary -- nextblockhash :: Maybe BlockHash
    <*> return 123 -- QC.generate QC.arbitrary -- strippedsize :: Word64
    <*> pure (Block.blockHeaderSize foundBlock) -- size :: Word64
    <*> pure (Block.blockHeaderWeight foundBlock) -- weight :: Word64

addReorganizations
  :: [Int]
  -> Vector Block.BlockHeader
  -> IO (Either Failure (Vector Block.BlockHeader, Vector Block.BlockHeader))
addReorganizations reorgOffsets plainBlockChain =
    let name = "addReorganizations"
    in runExceptPrefixTF name <! do
  when (reorgChunkSize < minimumSizeOfReorg) <!
    throwE <! Internal "reorgChunkSize < minimumSizeOfReorg"
  (chunks, reorganizedBlocks, unprocessed, _) <- foldM mkChainChunk
    ([], V.empty, plainBlockChain, 0) reorgOffsets
  return (V.concat <! List.reverse (unprocessed:chunks), reorganizedBlocks)
  where
    blockChainSize = V.length plainBlockChain
    reorgCounts = List.length reorgOffsets
    reorgChunkSize = blockChainSize `div` reorgCounts
    maxReorgOffset = List.foldl' max (List.head reorgOffsets) (List.tail reorgOffsets)
    minimumSizeOfReorg = maxReorgOffset + 1
    mkChainChunk !(acc, reorginizedBlocksAcc, blockChainToProcess, startIdx) !offset = do
      (chunkWithHeadNewBlockAndTail, newBlocks) <- getChunkWithHeadNewBlockAndTail
      return
        ( chunkWithHeadNewBlockAndTail:acc
        , V.concat [reorginizedBlocksAcc, newBlocks]
        , rest
        , startIdx + reorgChunkSize
        )
      where
        (initialChunk,rest) = V.splitAt reorgChunkSize blockChainToProcess
        (chainPrefix, lastElementInChain) = V.splitAt (reorgChunkSize - 1) initialChunk
        getChunkWithHeadNewBlockAndTail = do
          lastBlockInChain <- exceptTMaybeT (Internal "lastElementInChain !? 0")
            <! return <! lastElementInChain !? 0
          offsetOfBlockToReplace <- let
              rawUnchecked = V.length chainPrefix - offset
            in if rawUnchecked < 1
              then throwE
                <! Internal
                <! ( "rawUnchecked < 1: "
                   <> tshow (V.length chainPrefix)
                   <> ", "
                   <> tshow offset
                   <> ", "
                   <> tshow reorgChunkSize
                   )
              else return rawUnchecked
          blockToReplace <- exceptTMaybeT (Internal "chainPrefix !? offsetOfBlockToReplace")
            <! return <! chainPrefix !? offsetOfBlockToReplace
          blockPrefixToBlockToReplace <-
            exceptTMaybeT (Internal "chainPrefix !? (offsetOfBlockToReplace - 1)")
              <! return <! chainPrefix !? (offsetOfBlockToReplace - 1)
          let
              newBranchStartTime = Block.blockHeaderTimestamp lastBlockInChain + 10
              newBranchStartHeight = Block.blockHeaderHeight blockToReplace
              newBranchEndHeight = Block.blockHeaderHeight blockToReplace
                + verifyNatural offset
          randomBranch <- lift <! V.take offset
            <$> generateBlockChain newBranchStartTime
              newBranchStartHeight newBranchEndHeight
          let
              newBranchConnectedToInitialBranch = V.modify
                ( connectBranchTo blockPrefixToBlockToReplace
                  ( Block.blockHeaderTimestamp lastBlockInChain
                    - fromIntegral (offset + 1) * 10
                  )
                )
                randomBranch
          theLastBlockOfTheNewBranch <- exceptTMaybeT
            (Internal "newBranchConnectedToInitialBranch !? (offset - 1)")
              <! return <! newBranchConnectedToInitialBranch !? (offset - 1)
          let
             newLastBlockInChunk = lastBlockInChain
               { Block.blockHeaderPreviousblockhash = Just
                 <! Block.blockHeaderHash theLastBlockOfTheNewBranch
               }
          return
            ( V.concat
              [ chainPrefix
              , newBranchConnectedToInitialBranch
              , V.singleton newLastBlockInChunk
              ]
            , V.concat
              [ newBranchConnectedToInitialBranch
              , V.singleton newLastBlockInChunk
              ]
            )
        connectBranchTo block newBranchStartTime v = do
          theFirstBlock <- VM.read v 0
          VM.write v 0 <! theFirstBlock
            { Block.blockHeaderPreviousblockhash = Just
              <! Block.blockHeaderHash block
            }
          void <! foldM syncTimestampsToBeJustALittleNewerThanOriginalBlocks
            newBranchStartTime
            [ 0 .. vSz - 1 ]
          where
          vSz = VM.length v
          syncTimestampsToBeJustALittleNewerThanOriginalBlocks blockStartTime idx = do
                newBlock <- VM.read v idx
                VM.write v idx <! newBlock
                  { Block.blockHeaderTimestamp =
                    blockStartTime + 10
                  }
                return <! blockStartTime + 10

