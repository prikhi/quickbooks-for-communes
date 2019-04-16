module Router where

import Prelude
import Control.Monad.State (class MonadState)
import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq (genericEq)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pages.NewCompany as NewCompany
import Pages.NewTrip as NewTrip
import Routing.Match (Match, lit, root, end)
import Web.UIEvent.MouseEvent as ME

import App
    ( AppM
    , class Navigation, newUrl
    , class PreventDefaultClick, preventClick
    , class PreventDefaultEnter
    , class PreventDefaultSubmit
    , class ManageObjectURLs
    , class LogToConsole
    , class DateTime
    , class FocusElement
    )
import Server (class Server)

data Route
    = Home
    | NewCompany
    | NewTrip

derive instance genericRoute :: Generic Route _
instance showRoute :: Show Route where
    show = genericShow
instance eqRoute :: Eq Route where
    eq = genericEq

routeName :: Route -> String
routeName = case _ of
    Home ->
        "Home"
    NewCompany ->
        "New Company"
    NewTrip ->
        "Add a Trip"

type State
    = { currentPage :: Route }

data Input a
    = Goto Route a


data Query a
    = UpdateRoute Route a
    -- ^ Set the Application Route. Called on URL changes.

data Action
    = NavClick Route ME.MouseEvent
    -- ^ Prevent the MouseEvent, Change the Route & URL



router :: Match Route
router =
    root *> oneOf
        [ home
        , newCompany
        , newTrip
        ]
  where
    home = Home <$ end
    newCompany = NewCompany <$ lit "new-company" <* end
    newTrip = NewTrip <$ lit "trips" <* lit "add" <* end

reverse :: Route -> String
reverse = case _ of
    Home -> "/"
    NewCompany -> "/new-company/"
    NewTrip -> "/trips/add/"

component :: forall i o. H.Component HH.HTML Query i o AppM
component = H.mkComponent
    { initialState: const initial
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = eval
        , handleQuery = evalQuery
        }
    }

initial :: State
initial = { currentPage : Home }


-- | Slots for each page.
type ChildSlots =
    ( newCompany :: H.Slot NewCompany.Query Void Unit
    , newTrip :: H.Slot NewTrip.Query Void Unit
    )

-- | Selector for the NewCompany Slot.
_newCompany :: SProxy "newCompany"
_newCompany = SProxy

-- | Selector for the NewTrip Slot.
_newTrip :: SProxy "newTrip"
_newTrip = SProxy

-- | Handle Navigation clicks & URL updates.
eval :: forall m
     . MonadState State m
    => Navigation m
    => PreventDefaultClick m
    => Action -> m Unit
eval = case _ of
    NavClick route ev -> do
       preventClick ev
       newUrl $ reverse route

evalQuery :: forall m a g o. Query a -> H.HalogenM State Action g o m (Maybe a)
evalQuery = case _ of
    UpdateRoute route next -> do
        H.modify_ (_ { currentPage = route })
        pure $ Just next

-- | Render the application.
render :: forall m
    . PreventDefaultSubmit m
   => PreventDefaultEnter m
   => PreventDefaultClick m
   => Server m
   => ManageObjectURLs m
   => LogToConsole m
   => DateTime m
   => FocusElement m
   => State -> H.ComponentHTML Action ChildSlots m
render { currentPage } =
    HH.div_
        [ renderHeader currentPage
        , HH.div [HP.class_ $ H.ClassName "content"]
            [ HH.h1_ [HH.text $ routeName currentPage]
            , renderPage currentPage
            ]
        ]

-- | Render the page header/navigation.
-- | TODO: Company Selector
renderHeader :: forall a. Route -> HH.HTML a Action
renderHeader currentPage =
    HH.nav_
        [ brandLink
        , navLink NewTrip
        , navLink NewCompany
        ]
  where
    brandLink :: HH.HTML a Action
    brandLink =
        HH.a
            [ HP.class_ $ H.ClassName "brand"
            , HP.href $ reverse Home
            , HE.onClick $ Just <<< NavClick Home
            ]
            [ HH.text "AcornAccounting" ]
    navLink =
        navigationLink currentPage

-- | Render the page's component.
renderPage :: forall m
    . PreventDefaultSubmit m
   => PreventDefaultEnter m
   => PreventDefaultClick m
   => Server m
   => ManageObjectURLs m
   => LogToConsole m
   => DateTime m
   => FocusElement m
   => Route -> H.ComponentHTML Action ChildSlots m
renderPage = case _ of
    Home ->
        HH.fromPlainHTML renderHomepage
    NewCompany ->
        HH.slot _newCompany unit NewCompany.component unit (const Nothing)
    NewTrip ->
        HH.slot _newTrip unit NewTrip.component unit (const Nothing)
  where
    liText t = HH.li_ [HH.text t]

-- | Render the static HTML for the home page.
renderHomepage :: HH.PlainHTML
renderHomepage =
    HH.p_
        [ HH.text "TODO: Throw a Chart of Accounts or Some Intro Text Here."
        ]

-- | Render a navigation link.
navigationLink :: forall a. Route -> Route -> HH.HTML a Action
navigationLink currentPage route =
    HH.a
        [ HP.href $ reverse route
        , HE.onClick $ Just <<< NavClick route
        , HP.classes $ if route == currentPage then [H.ClassName "active"] else []
        ]
        [ HH.span_ [ HH.text $ routeName route ] ]
