{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.API.V1 where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.Proxy
import           Servant.API
import           Servant.API.WebSocket (WebSocket)
import           Servant.Swagger(HasSwagger(..))
import           Data.Text                  (Text)

import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.API.V1.Account
import           Data.OpEnergy.API.V1.Block

-- | API specifications of a backend service for Swagger
type V1API
  = "ws"
    :> Description "websockets handler"
    :> WebSocket
  :<|> "register"
    :> Description "Registers new user and returns randomly generated account secret and account token.\n Account secret should be used for /login API encpoint.\n Account token should be used in the rest API calls as an authentication cookie"
    :> Post '[JSON] RegisterResult

  :<|> "login"
    :> ReqBody '[JSON] [AccountSecret]
    :> Description "Performs login with given account secret. Returns AccountToken value for being used with the rest API calls"
    :> Post '[JSON] [AccountToken]

  :<|> "strike"
    :> "mediantime"
    :> Description "Returns list of strikes created by mediantime"
    :> Get '[JSON] [TimeStrike]

  :<|> "strike"
    :> "block"
    :> "mediantime"
    :> Capture "block_height" BlockHeight
    :> Description "Returns list of strikes by a given block height"
    :> Get '[JSON] [TimeStrike]

  :<|> "strike"
    :> "mediantime"
    :> ReqBody '[JSON] CreateTimeStrikeRequest
    :> Description "Creates new strike defined by BlockHeight and NLockTime values"
    :> Post '[JSON] TimeStrike

  :<|> "slowfastguess"
    :> "mediantime"
    :> Capture "block_height" BlockHeight
    :> Capture "nlocktime" NLockTime
    :> Description ""
    :> Get '[JSON] [SlowFastGuess]

  :<|> "slowfastguess"
    :> "mediantime"
    :> ReqBody '[JSON] CreateSlowFastGuessRequest
    :> Description "Creates a slow/fast guess for a given time strike"
    :> Post '[JSON] SlowFastGuess

  :<|> "strikeshistory"
    :> "mediantime"
    :> Description "Returns list of archived time strikes  (ie, already fired and moved into an archive)"
    :> Get '[JSON] [TimeStrikeHistory]

  :<|> "slowfastresults"
    :> "mediantime"
    :> ReqBody '[JSON] [AccountToken]
    :> Capture "nlocktime" NLockTime
    :> Capture "block_height" BlockHeight
    :> Get '[JSON] [SlowFastResult]

  :<|> "user"
    :> "displayname"
    :> ReqBody '[JSON] PostUserDisplayNameRequest
    :> Description "Updates displayname for a given user"
    :> Post '[JSON] ()

  :<|> "statistics"
    :> Capture "blockheight" BlockHeight
    :> Capture "span" (Positive Int)
    :> Description "Calculates NBDR statistics for a given block height and span. NBDR here is ratio (span * 600 * 100) / (endBlockMedianTime - startBlockMediantime)."
    :> Get '[JSON] Statistics

  :<|> "oe"
    :> "block"
    :> Capture "hash" BlockHash
    :> Description "Returns block's header by a given block hash, including chainwork, that is missing from mempool's blocks' headers cache"
    :> Get '[JSON] BlockHeader

  :<|> "oe"
    :> "blockbyheight"
    :> Capture "height" BlockHeight
    :> Description "Returns block's header by a given block height"
    :> Get '[JSON] BlockHeader

  :<|> "oe"
    :> "blocksbyblockspan"
    :> Capture "startBlockHeight" BlockHeight
    :> Capture "span" (Positive Int)
    :> QueryParam "numberOfSpan" (Positive Int)
    :> Description "Returns list of blocks' headers by a given block span. Answer format: [ [startBlockHeight, startBlockHeight + span], [startBlockHeight + span, ...], ... ]. If numberOfSpan is missing, then it will provide blockspans until the current tip."
    :> Get '[JSON] [[BlockHeader]]

  :<|> "oe"
    :> "blockswithnbdrbyblockspan"
    :> Capture "startBlockHeight" BlockHeight
    :> Capture "span" (Positive Int)
    :> QueryParam "numberOfSpan" (Positive Int)
    :> Description "Returns list of start and end blocks' headers and their nbdr for each appropriate block span. NBDR here is ratio (span * 600 * 100) / (endBlockMedianTime - startBlockMediantime). If numberOfSpan is missing, then it will provide blockspans until the current tip."
    :> Get '[JSON] [BlockSpanHeadersNbdr]

  :<|> "oe"
    :> "blockspanlist"
    :> Capture "startBlockHeight" BlockHeight
    :> Capture "span" (Positive Int)
    :> Capture "numberOfSpan" (Positive Int)
    :> Description "Returns list of spans started from startBlockHeight of size span and numberOfSpan length "
    :> Get '[JSON] [BlockSpan]

  :<|> "oe"
    :> "git-hash"
    :> Description "returns short hash of commit of the op-energy git repo that had been used to build backend"
    :> Get '[JSON] GitHashResponse


type FakeWSAPI = Get '[JSON] ()
instance HasSwagger WebSocket where
  toSwagger _ = toSwagger api
    where
      api :: Proxy FakeWSAPI
      api = Proxy

data GitHashResponse = GitHashResponse
  { gitCommitHash :: Text
  }
  deriving (Show, Generic, Typeable)
instance ToJSON GitHashResponse
instance FromJSON GitHashResponse
instance ToSchema GitHashResponse where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "GitHashResponse schema"
    & mapped.schema.example ?~ toJSON defaultGitHashResponse
defaultGitHashResponse :: GitHashResponse
defaultGitHashResponse = GitHashResponse
  { gitCommitHash = "12345678"
  }

data BlockSpanHeadersNbdr = BlockSpanHeadersNbdr
  { startBlock :: BlockHeader
  , endBlock :: BlockHeader
  , nbdr :: Double
  }
  deriving (Show, Generic, Typeable)
instance ToJSON   BlockSpanHeadersNbdr
instance FromJSON BlockSpanHeadersNbdr
instance ToSchema BlockSpanHeadersNbdr where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "BlockSpanHeadersNbdr schema"
    & mapped.schema.example ?~ toJSON defaultBlockSpanHeadersNbdr
defaultBlockSpanHeadersNbdr :: BlockSpanHeadersNbdr
defaultBlockSpanHeadersNbdr = BlockSpanHeadersNbdr
  { startBlock = defaultBlockHeader
  , endBlock = defaultBlockHeader
  , nbdr = 100.0
  }

data NbdrStatistics = NbdrStatistics
  { avg :: Double
  , stddev :: Double
  }
  deriving (Show, Generic, Typeable)

defaultNbdrStatistics :: NbdrStatistics
defaultNbdrStatistics = NbdrStatistics
  { avg = 1
  , stddev = 1
  }

instance ToJSON NbdrStatistics
instance FromJSON NbdrStatistics
instance ToSchema NbdrStatistics where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "NbdrStatistics schema"
    & mapped.schema.example ?~ toJSON defaultNbdrStatistics

data Statistics = Statistics
  { nbdr :: NbdrStatistics
  }
  deriving (Show, Generic, Typeable)

defaultStatistics :: Statistics
defaultStatistics = Statistics
  { nbdr = defaultNbdrStatistics
  }

instance ToJSON Statistics
instance FromJSON Statistics
instance ToSchema Statistics where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "Statistics schema"
    & mapped.schema.example ?~ toJSON defaultStatistics


data SlowFastResult = SlowFastResult
  { guess :: Guess
  , result :: GuessResult
  , blockHeight :: BlockHeight
  , nLockTime :: NLockTime
  , creationTime :: Positive Int
  }
  deriving (Show, Generic, Typeable)

defaultSlowFastResult :: SlowFastResult
defaultSlowFastResult = SlowFastResult
  { guess = Slow
  , result = GuessResultRight
  , blockHeight = defaultBlockHeight
  , nLockTime = verifyNatural 1
  , creationTime = verifyPositive 1
  }

instance ToJSON SlowFastResult
instance FromJSON SlowFastResult
instance ToSchema SlowFastResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "SlowFastResult schema"
    & mapped.schema.example ?~ toJSON defaultSlowFastResult

data RegisterResult = RegisterResult
  { accountSecret :: AccountSecret
  , accountToken  :: AccountToken
  }
  deriving (Show, Generic, Typeable)

defaultRegisterResult :: RegisterResult
defaultRegisterResult = RegisterResult defaultAccountSecret defaultAccountToken

instance ToJSON RegisterResult
instance FromJSON RegisterResult
instance ToSchema RegisterResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "RegisterResult schema"
    & mapped.schema.example ?~ toJSON defaultRegisterResult


data TimeStrikeHistory = TimeStrikeHistory
  { owner :: Text
  , blockHeight  :: BlockHeight
  , nLockTime    :: NLockTime
  , mediantime   :: Positive Int
  , creationTime :: Positive Int
  , archiveTime  :: Positive Int
  , wrongResults :: Natural Int
  , rightResults :: Natural Int
  }
  deriving (Show, Generic, Typeable)
instance ToJSON TimeStrikeHistory
instance FromJSON TimeStrikeHistory
instance ToSchema TimeStrikeHistory where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "TimeStrikeHistory schema"
    & mapped.schema.example ?~ toJSON defaultTimeStrikeHistory

defaultTimeStrikeHistory :: TimeStrikeHistory
defaultTimeStrikeHistory = TimeStrikeHistory
  { owner = "userName"
  , blockHeight = defaultBlockHeight
  , nLockTime = verifyNatural 1
  , mediantime = verifyPositiveInt 1000
  , creationTime = verifyPositiveInt 1000
  , archiveTime = verifyPositive 1001
  , wrongResults = verifyNaturalInt 0
  , rightResults = verifyNaturalInt 0
  }


data GuessResult = GuessResultRight | GuessResultWrong
  deriving (Show, Generic, Typeable, Enum)
defaultResult :: GuessResult
defaultResult = GuessResultWrong

instance Bounded GuessResult where
  minBound = GuessResultRight
  maxBound = GuessResultWrong
instance ToJSON GuessResult where
  toJSON GuessResultRight = toJSON ("right":: Text)
  toJSON GuessResultWrong = toJSON ("wrong":: Text)
instance FromJSON GuessResult where
  parseJSON = withText "Result" $ \v-> return $
    case v of
      "right" -> GuessResultRight
      "wrong" -> GuessResultWrong
      _ -> error "FromJSON GuessResult: unimplemented"
instance ToSchema GuessResult where
  declareNamedSchema _ = return $ NamedSchema (Just "Result") $ mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ (map toJSON $ enumFrom GuessResultRight)

data Guess = Slow | Fast
  deriving (Show, Generic, Typeable, Enum)
defaultGuess :: Guess
defaultGuess = Fast

instance Bounded Guess where
  minBound = Slow
  maxBound = Fast
instance ToJSON Guess where
  toJSON Slow = toJSON ("slow":: Text)
  toJSON Fast = toJSON ("fast":: Text)
instance FromJSON Guess where
  parseJSON = withText "Guess" $ \v-> return $
    case v of
      "slow" -> Slow
      "fast" -> Fast
      _ -> error "FromJSON Guess: unimplemented"
instance ToSchema Guess where
  declareNamedSchema _ = return $ NamedSchema (Just "Guess") $ mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ (map toJSON $ enumFrom Slow)

data PostUserDisplayNameRequest = PostUserDisplayNameRequest
  { account_token :: AccountToken
  , display_name :: Text
  }
  deriving (Show, Generic, Typeable)
defaultPostUserDisplayNameRequest :: PostUserDisplayNameRequest
defaultPostUserDisplayNameRequest = PostUserDisplayNameRequest
  { account_token = defaultAccountToken
  , display_name = "newUserName"
  }
instance ToJSON PostUserDisplayNameRequest
instance FromJSON PostUserDisplayNameRequest
instance ToSchema PostUserDisplayNameRequest where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "PostUserDisplayNameRequest schema"
    & mapped.schema.example ?~ toJSON defaultPostUserDisplayNameRequest

data CreateSlowFastGuessRequest = CreateSlowFastGuessRequest
  { account_token :: AccountToken
  , guess :: Guess
  , nlocktime :: NLockTime
  , block_height :: BlockHeight
  }
  deriving (Show, Generic, Typeable)

defaultCreateSlowFastGuessRequest :: CreateSlowFastGuessRequest
defaultCreateSlowFastGuessRequest = CreateSlowFastGuessRequest
  { account_token = defaultAccountToken
  , guess = defaultGuess
  , nlocktime = verifyNatural 1
  , block_height = defaultBlockHeight
  }
instance ToJSON CreateSlowFastGuessRequest
instance FromJSON CreateSlowFastGuessRequest
instance ToSchema CreateSlowFastGuessRequest where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "CreateSlowFastGuessRequest schema"
    & mapped.schema.example ?~ toJSON defaultCreateSlowFastGuessRequest

data SlowFastGuess = SlowFastGuess
  { guess :: Guess
  , blockHeight :: BlockHeight
  , nLockTime :: NLockTime
  , creationTime :: Positive Int
  , userName :: Text
  , userId :: Natural Int
  }
  deriving (Show, Generic, Typeable)

defaultSlowFastGuess :: SlowFastGuess
defaultSlowFastGuess = SlowFastGuess
  { guess = Slow
  , blockHeight = defaultBlockHeight
  , nLockTime = verifyNatural 1
  , creationTime = verifyPositive 1
  , userName = "someUserName"
  , userId = verifyNaturalInt 0
  }

instance ToJSON SlowFastGuess
instance FromJSON SlowFastGuess
instance ToSchema SlowFastGuess where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "SlowFastGuess schema"
    & mapped.schema.example ?~ toJSON defaultSlowFastGuess

data CreateTimeStrikeRequest = CreateTimeStrikeRequest
  { account_token :: AccountToken
  , nlocktime :: NLockTime
  , block_height :: BlockHeight
  }
  deriving (Show, Generic, Typeable)

defaultCreateTimeStrikeRequest :: CreateTimeStrikeRequest
defaultCreateTimeStrikeRequest = CreateTimeStrikeRequest defaultAccountToken (verifyNatural 1) defaultBlockHeight

instance ToJSON CreateTimeStrikeRequest
instance FromJSON CreateTimeStrikeRequest
instance ToSchema CreateTimeStrikeRequest where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "CreateTimeStrikeRequest schema"
    & mapped.schema.example ?~ toJSON defaultCreateTimeStrikeRequest

type NLockTime = Natural Int

data TimeStrike = TimeStrike
  { blockHeight  :: BlockHeight
  , nLockTime    :: NLockTime
  , creationTime :: Positive Int
  }
  deriving (Show, Generic, Typeable)
instance ToJSON TimeStrike
instance FromJSON TimeStrike
instance ToSchema TimeStrike where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "TimeStrike schema"
    & mapped.schema.example ?~ toJSON defaultTimeStrike

defaultTimeStrike :: TimeStrike
defaultTimeStrike = TimeStrike
  { blockHeight = defaultBlockHeight
  , nLockTime = verifyNatural 1
  , creationTime = 1000
  }



