module Router where

import Prelude
import Control.Monad.State (class MonadState)
import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq (genericEq)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Routing.Match (Match, lit, root, end)
import Web.UIEvent.MouseEvent as ME

import App (AppM, class Navigation, newUrl, class PreventDefaultClick, preventClick)

data Route
    = Home
    | NewCompany

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

type State
    = { currentPage :: Route }

data Input a
    = Goto Route a

data Query a
    = NavClick Route ME.MouseEvent a
    -- ^ Prevent the MouseEvent, Change the Route & URL
    | UpdateRoute Route a
    -- ^ Set the Application Route. Called on URL changes.


router :: Match Route
router =
    root *> oneOf
        [ home
        , newCompany
        ]
  where
    home = Home <$ end
    newCompany = NewCompany <$ lit "new-company" <* end

reverse :: Route -> String
reverse = case _ of
    Home -> "/"
    NewCompany -> "/new-company/"

component :: H.Component HH.HTML Query Unit Void AppM
component = H.parentComponent
    { initialState: const initial
    , render
    , eval
    , receiver: const Nothing
    }

initial :: State
initial = { currentPage : Home }

data PageSlot
    = NewCompanySlot

derive instance eqPageSlot :: Eq PageSlot
derive instance ordPageSlot :: Ord PageSlot

-- | Handle Navigation clicks & URL updates.
eval :: forall m
     . MonadState State m
    => Navigation m
    => PreventDefaultClick m
    => Query ~> m
eval = case _ of
    (NavClick route ev next) -> do
       preventClick ev
       newUrl $ reverse route
       pure next
    (UpdateRoute route next) ->
       updatePage route *> pure next
  where
    updatePage :: forall n. MonadState State n => Route -> n Unit
    updatePage route = H.modify_ (_ { currentPage = route })

render :: forall q m. State -> H.ParentHTML Query q PageSlot m
render { currentPage } =
    HH.div_
        [ renderHeader currentPage
        , HH.div [HP.class_ $ H.ClassName "content"]
            [ HH.h1_ [HH.text $ routeName currentPage]
            , renderPage currentPage
            ]
        ]

-- | TODO: Company Selector
renderHeader :: forall a. Route -> H.HTML a Query
renderHeader currentPage =
    let navLink = navigationLink currentPage in
    HH.nav [ HP.class_ $ H.ClassName "navbar" ]
        [ HH.a
            [ HP.class_ $ H.ClassName "brand"
            , HP.href $ reverse Home
            , HE.onClick $ HE.input $ NavClick Home
            ]
            [ HH.text "Acorn Accounting" ]
        , navLink NewCompany
        ]

-- | TODO: Use page slots to render
renderPage :: forall a b. Route -> H.HTML a b
renderPage = case _ of
    Home -> HH.p_
        [ HH.text "TODO: Throw a Chart of Accounts or Some Intro Text Here." ]
    NewCompany -> HH.p_
        [ HH.text "TODO: Form for adding new company w/ fields for:"
        , HH.ul_
            [ liText "Company Name"
            , liText "Username"
            , liText "Password"
            , liText "Company File?"
            ]
        ]
  where
    liText t = HH.li_ [HH.text t]

-- | Render a navigation link.
navigationLink :: forall a. Route -> Route -> H.HTML a Query
navigationLink currentPage route =
    HH.a
        [ HP.href $ reverse route
        , HE.onClick $ HE.input $ NavClick route
        , HP.classes $ if route == currentPage then [H.ClassName "active"] else []
        ]
        [ HH.text $ routeName route ]
