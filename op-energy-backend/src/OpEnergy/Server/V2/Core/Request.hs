module OpEnergy.Server.V2.Core.Request
  ( Request(..)
  , allPossibleRequests
  ) where

import           OpEnergy.Server.V2.Core.Call(Call0(..))

data Request
  = Ping (Call0 () )
  deriving (Show)
-- this should be the last request
maxRequest :: Request
maxRequest = Ping (Call0 Nothing)

instance Bounded Request where
  minBound = Ping (Call0 Nothing)
  maxBound = maxRequest

allPossibleRequests :: [Request]
allPossibleRequests = [ minBound .. maxBound ]

instance Enum Request where
  toEnum 0 = (Ping (Call0 Nothing))
  toEnum v = error ("toEnum Request: unsupported value" ++ show v)
  fromEnum (Ping _ ) = 0

-- WARNING: we check only constructors, not the arguments!
instance Eq Request where
  l == r = fromEnum l == fromEnum r

-- WARNING: we check only constructors, not the arguments!
instance Ord Request where
  compare l r = compare (fromEnum l) (fromEnum r)
