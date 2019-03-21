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
import Effect.Class.Console as Console
import Foreign (unsafeToForeign)
import Routing.PushState (PushStateInterface)
import Web.Event.Event as E
import Web.HTML.Event.EventTypes as ET
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes as MET


-- Application Monad

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


-- Application Environment

data AppEnv
    = Env
        { nav :: PushStateInterface
        }

class HasNav a where
    getNav :: a -> PushStateInterface

instance hasNavEnv :: HasNav AppEnv where
    getNav (Env e) = e.nav


-- Functionality-based Typeclasses

-- Navigation

class Monad m <= Navigation m where
    newUrl :: String -> m Unit

instance navEnv :: (HasNav e, MonadEffect m, MonadAsk e m) => Navigation m where
    newUrl url = do
        nav <- asks getNav
        liftEffect $ nav.pushState (unsafeToForeign {}) url

-- Event Prevention

class Monad m <= PreventDefaultClick m where
    preventClick :: ME.MouseEvent -> m Unit

instance prevDefClickEffect :: (MonadEffect m) => PreventDefaultClick m where
    preventClick ev =
        let event = ME.toEvent ev in
        if E.type_ event == MET.click then
            liftEffect $ E.preventDefault event
        else
            pure unit


class Monad m <= PreventDefaultSubmit m where
    preventSubmit :: E.Event -> m Unit

instance prevDefSubmitEffect :: (MonadEffect m) => PreventDefaultSubmit m where
    preventSubmit ev =
        if E.type_ ev == ET.submit then
            liftEffect $ E.preventDefault ev
        else
            pure unit

-- Logging

class Monad m <= LogToConsole m where
    logShow :: forall a. Show a => a -> m Unit
    log :: String -> m Unit

instance logToConsoleEffect :: MonadEffect m => LogToConsole m where
    logShow = Console.logShow
    log = Console.log
