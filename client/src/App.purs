module App where

import Prelude

import Control.Monad.Reader
    ( ReaderT
    , runReaderT
    , class MonadAsk
    , asks
    )
import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.JSDate as JSDate
import Data.Maybe (Maybe)
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
import Effect.Now (nowDateTime)
import Foreign (unsafeToForeign)
import Halogen as H
import Routing.PushState (PushStateInterface)
import Web.Event.Event as E
import Web.File.Blob (Blob)
import Web.File.Url as F
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Event.EventTypes as ET
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
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
--
-- These should be defined for our base monad AppM, as well as the HalogenM
-- wrapper that the Halogen.component & Halogen.parentComponent return.

-- Navigation

class Monad m <= Navigation m where
    newUrl :: String -> m Unit

instance navEnv :: Navigation AppM where
    newUrl url = do
        nav <- asks getNav
        liftEffect $ nav.pushState (unsafeToForeign {}) url

instance navHalogen :: Navigation m
    => Navigation (H.HalogenM s f g p o m) where
    newUrl = H.lift <<< newUrl


-- Event Prevention

class Monad m <= PreventDefaultClick m where
    preventClick :: ME.MouseEvent -> m Unit

instance prevDefClickApp :: PreventDefaultClick AppM where
    preventClick ev =
        let event = ME.toEvent ev in
        if E.type_ event == MET.click then
            liftEffect $ E.preventDefault event
        else
            pure unit

instance prevDefClickHalogen :: PreventDefaultClick m
    => PreventDefaultClick (H.HalogenM s f g p o m) where
    preventClick = H.lift <<< preventClick


class Monad m <= PreventDefaultEnter m where
    -- | Prevent the default event on keydown events for the Enter key.
    -- | Returns true if the event was prevented.
    preventEnter :: KE.KeyboardEvent -> m Boolean

instance prevDefEnterApp :: PreventDefaultEnter AppM where
    preventEnter ev =
        let event = KE.toEvent ev in
        if E.type_ event == KET.keydown && KE.key ev == "Enter" then
            liftEffect (E.preventDefault event) *> pure true
        else
            pure false

instance prevDefEnterHalogen :: PreventDefaultEnter m
    => PreventDefaultEnter (H.HalogenM s f g p o m) where
    preventEnter = H.lift <<< preventEnter


class Monad m <= PreventDefaultSubmit m where
    preventSubmit :: E.Event -> m Unit

instance prevDefSubmitApp :: PreventDefaultSubmit AppM where
    preventSubmit ev =
        if E.type_ ev == ET.submit then
            liftEffect $ E.preventDefault ev
        else
            pure unit

instance prevDefSubmitHalogen :: PreventDefaultSubmit m
    => PreventDefaultSubmit (H.HalogenM s f g p o m) where
    preventSubmit = H.lift <<< preventSubmit


-- Logging

class Monad m <= LogToConsole m where
    logShow :: forall a. Show a => a -> m Unit
    log :: String -> m Unit

instance logToConsoleApp :: LogToConsole AppM where
    logShow = Console.logShow
    log = Console.log

instance logToConsoleHalogen :: LogToConsole m
    => LogToConsole (H.HalogenM s f g p o m) where
    logShow = H.lift <<< logShow
    log = H.lift <<< log


-- Blob Object URLs

class Monad m <= ManageObjectURLs m where
    createObjectURL :: Blob -> m String
    revokeObjectURL :: String -> m Unit

instance manageObjectUrlsApp :: ManageObjectURLs AppM where
    createObjectURL = H.liftEffect <<< F.createObjectURL
    revokeObjectURL = H.liftEffect <<< F.revokeObjectURL

instance manageObjectUrlsHalogen :: ManageObjectURLs m
    => ManageObjectURLs (H.HalogenM s f g p o m) where
    createObjectURL = H.lift <<< createObjectURL
    revokeObjectURL = H.lift <<< revokeObjectURL


-- Dates

class Monad m <= DateTime m where
    parseDate :: String -> m (Maybe Date)
    now :: m DateTime

instance dateTimeApp :: DateTime AppM where
    parseDate str =
        JSDate.toDate <$> (H.liftEffect $ JSDate.parse str)
    now =
        H.liftEffect nowDateTime

instance dateTimeHalogen :: DateTime m
    => DateTime (H.HalogenM s f g p o m) where
    parseDate = H.lift <<< parseDate
    now = H.lift $ now


-- Focusing

class Monad m <= FocusElement m where
    focusElement :: HTMLElement.HTMLElement -> m Unit

instance focusApp :: FocusElement AppM where
    focusElement = H.liftEffect <<< HTMLElement.focus

instance focusHalogen :: FocusElement m
    => FocusElement (H.HalogenM s f g p o m) where
    focusElement = H.lift <<< focusElement
