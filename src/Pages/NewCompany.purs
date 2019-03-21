module Pages.NewCompany
    ( component
    , Query
    ) where

{- | The page for add a New QuickBooks Company. This presents a form for the
   | Company which, on submission, displays a download link for the QuickBooks
   | WebConnector's @qwc@ config file.

-}

import Prelude

import Control.Monad.State (class MonadState)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as E

import App
    ( class PreventDefaultSubmit, preventSubmit
    , class LogToConsole, logShow
    )


component :: forall m i o
    . MonadEffect m
   => PreventDefaultSubmit m
   => LogToConsole m
   => H.Component HH.HTML Query i o m
component = H.component
    { initialState: const initial
    , render
    , eval
    , receiver: const Nothing
    }

-- | The form data.
type State =
    { name :: String
    , username :: String
    , password :: String
    }

-- | Start the form as blank.
initial :: State
initial =
    { name: ""
    , username: ""
    , password: ""
    }


-- | Form input & submission events.
data Query a
    = InputName String a
    | InputUser String a
    | InputPass String a
    | SubmitForm E.Event a
    -- ^ Log the current form fields to the console.


-- | Update & submit the form.
eval :: forall m
    . MonadState State m
   => PreventDefaultSubmit m
   => LogToConsole m
   => Query ~> m
eval = case _ of
    InputName str next -> do
        H.modify_ (_ { name = str })
        pure next
    InputUser str next -> do
        H.modify_ (_ { username = str })
        pure next
    InputPass str next -> do
        H.modify_ (_ { password = str })
        pure next
    SubmitForm ev next -> do
        preventSubmit ev
        H.get >>= logShow
        -- TODO: Validation & Form Submission
        pure next


-- | Render the New Company page.
render :: State -> H.ComponentHTML Query
render st =
    HH.div_
        [ HH.p_
            [ HH.text "Use this form to add a new QuickBooks Company. A Company \
                \will sync to a specific QuickBooks file, pulling it's account \
                \data and allowing entries to be created for it."
            ]
        , HH.p_
            [ HH.text "After creating a Company, you will be given a configuration \
                \file for the QuickBooks WebConnector. You will need to open the \
                \company in QuickBooks, and add the config to the WebConnector to \
                \initialize the data."
            ]
        , HH.p_ [ HH.text "TODO: Validate & post form to backend on submit." ]
        , HH.form [ HE.onSubmit $ HE.input SubmitForm ]
            [ input "Company Name" HP.InputText st.name InputName
            , input "Username" HP.InputText st.username InputUser
            , input "Password" HP.InputPassword st.password InputPass
            , submitButton "Add Company"
            ]
        ]
  where
    liText t = HH.li_ [ HH.text t ]

-- | Render a standard `HH.input` element with a label.
-- |
-- | TODO: add to Forms module?
input :: forall p i. String -> HP.InputType -> String -> (String -> Unit -> i Unit) -> HH.HTML p (i Unit)
input label type_ value action =
    HH.label_
        [ HH.div_ [ HH.text label ]
        , HH.input
            [ HP.type_ type_
            , HP.value value
            , HP.required true
            , HE.onValueChange $ HE.input action
            ]
        ]

-- | Render a standard `HH.button` element.
-- |
-- | TODO: add to Forms module?
button :: forall p i. String -> HP.ButtonType -> H.ClassName -> (Unit -> i Unit) -> HH.HTML p (i Unit)
button label type_ class_ action =
    HH.button
        [ HE.onClick $ HE.input_ action, HP.class_ class_, HP.type_ type_ ]
        [ HH.text label ]

-- | Render a button to submit it's form. Uses the primary button styling.
-- |
-- | TODO: add to Forms module?
submitButton :: forall p i. String -> HH.HTML p i
submitButton label =
    HH.button
        [ HP.class_ (H.ClassName "primary"), HP.type_ HP.ButtonSubmit ]
        [ HH.text label ]
