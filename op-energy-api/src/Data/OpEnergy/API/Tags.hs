{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances         #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE Trustworthy         #-}

module Data.OpEnergy.API.Tags where

import           Control.Lens
import           Data.HashSet.InsOrd
import           Data.Swagger
import           Data.Text
import           Data.Typeable       (Typeable)
import           GHC.TypeLits        (KnownSymbol, Symbol, symbolVal)
import           Servant             hiding (Context)
import           Servant.Swagger
import           Servant.Foreign.Internal
import           Servant.Client

data Tags (sym :: Symbol)
    deriving (Typeable)

instance HasClient m api => HasClient m (Tags tags :> api) where
  type Client m (Tags tags :> api ) = Client m api
  clientWithRoute proxy _ = clientWithRoute proxy (Proxy :: Proxy api)
  hoistClientMonad proxy _ = hoistClientMonad proxy (Proxy :: Proxy api)


instance HasServer api ctx => HasServer (Tags tags :> api) ctx where
  type ServerT (Tags tags :> api) m = ServerT api m
  route _ = route (Proxy :: Proxy api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api)

instance (KnownSymbol tags, HasSwagger api) => HasSwagger (Tags tags :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api)
    & allOperations.tags %~ union (fromList [pack (symbolVal (Proxy :: Proxy tags))])

instance
    ( KnownSymbol tags
    , HasForeign NoTypes NoContent api
    )
    =>
    HasForeign NoTypes NoContent (Tags tags :> api) where
  type Foreign NoContent (Tags tags :> api) = Foreign NoContent api
  foreignFor lang ftype Proxy req = foreignFor lang ftype (Proxy :: Proxy api) req

instance
    ( KnownSymbol tags
    , GenerateList NoContent (Foreign NoContent api)
    , HasForeign NoTypes NoContent api
    )
    =>
    GenerateList NoContent (Tags tags :> api ) where
  generateList _ =
    generateList  $!
      foreignFor
        (Proxy :: Proxy NoTypes)
        (Proxy :: Proxy NoContent)
        (Proxy :: Proxy api)
        defReq

