module App where

import Prelude

import Control.Monad.Reader 
    ( ReaderT
    , runReaderT
    , class MonadAsk
    , asks 
    )
import Data.Newtype 
    ( class Newtype
    , unwrap
    )
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class 
    ( class MonadEffect
    , liftEffect
    )
import Foreign (unsafeToForeign)
import Halogen as H
import Routing.PushState (PushStateInterface)

newtype AppM a
    = AppM (ReaderT AppEnv Aff a)

runAppM :: forall a. AppM a -> AppEnv -> Aff a
runAppM m = runReaderT (unwrap m)

derive instance newtypeAppM :: Newtype (AppM a) _
derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadAskAppM :: MonadAsk AppEnv AppM

data AppEnv
    = Env
        { nav :: PushStateInterface
        }

class HasNav a where 
    getNav :: a -> PushStateInterface

instance hasnavEnv :: HasNav AppEnv where 
    getNav (Env e) = e.nav

class Monad m <= Navigation m where 
    newUrl :: String -> m Unit

instance navEnv :: (HasNav e, MonadEffect m, MonadAsk e m) => Navigation m where 
    newUrl url = do
        nav <- asks getNav
        liftEffect $ nav.pushState (unsafeToForeign {}) url
