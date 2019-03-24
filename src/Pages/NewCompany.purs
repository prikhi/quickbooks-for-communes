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
import Data.Either (Either(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as E

import App
    ( class PreventDefaultSubmit, preventSubmit
    )
import Server (class Server, newCompanyRequest, NewCompanyData(..))
import Validation as V


component :: forall m i o
    . PreventDefaultSubmit m
   => Server m
   => H.Component HH.HTML Query i o m
component = H.component
    { initialState: const initial
    , render
    , eval
    , receiver: const Nothing
    }

-- | The form data.
type State =
    { name :: Maybe String
    , username :: Maybe String
    , password :: Maybe String
    , errors :: V.FormErrors
    }

-- | Start the form as blank.
initial :: State
initial =
    { name: Nothing
    , username: Nothing
    , password: Nothing
    , errors: V.empty
    }

validate :: State -> Either V.FormErrors NewCompanyData
validate st = V.toEither <<< map NewCompany $
        { name: _, user: _, password: _ }
            <$> V.validateNonEmpty "name" st.name
            <*> V.validateNonEmpty "username" st.username
            <*> V.validateNonEmpty "password" st.password


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
   => Server m
   => Query ~> m
eval = case _ of
    InputName str next -> do
        H.modify_ (_ { name = Just str })
        revalidate "name"
        pure next
    InputUser str next -> do
        H.modify_ (_ { username = Just str })
        revalidate "username"
        pure next
    InputPass str next -> do
        H.modify_ (_ { password = Just str })
        revalidate "password"
        pure next
    SubmitForm ev next -> do
        preventSubmit ev
        st <- H.get
        case validate st of
            Left errs ->
                H.modify_ (_ { errors = errs })
            Right ncd -> do
                H.modify_ (_ { errors = V.empty })
                -- TODO: Handle some response, like company id or the qwc file.
                newCompanyRequest ncd >>= \r -> case r.body of
                    Left errs ->
                        H.modify_ (_ { errors = errs })
                    Right _ ->
                        pure unit
        pure next
  where
    revalidate = V.revalidate validate


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
        , HH.form [ HE.onSubmit $ HE.input SubmitForm ]
            [ formErrors $ V.getFormErrors st.errors
            , input "Company Name" HP.InputText st.name InputName $ errors "name"
            , input "Username" HP.InputText st.username InputUser $ errors "username"
            , input "Password" HP.InputPassword st.password InputPass $ errors "password"
            , submitButton "Add Company"
            ]
        ]
  where
    liText t = HH.li_ [ HH.text t ]
    errors field = V.getFieldErrors field st.errors



-- Forms - TODO: stick in module

-- | Render the general errors for a form.
formErrors :: forall p i. Array String -> HH.HTML p i
formErrors errs =
    if not $ Array.null errs then
        HH.p [HP.class_ $ H.ClassName "form-errors"]
            [ HH.text "We encountered the following errors when processing \
                \your request. Please address them and try re-submitting the form:"
            , HH.ul_ $ map (\err -> HH.li_ [HH.text err]) errs
            ]
    else
        HH.text ""

-- | Render a standard `HH.input` element with a label and error list.
input :: forall p i. String -> HP.InputType -> Maybe String -> (String -> Unit -> i Unit) -> Array String -> HH.HTML p (i Unit)
input label type_ value action errors =
    HH.label errorClass
        [ HH.div_ [ HH.text label ]
        , HH.input $
            [ HP.type_ type_
            , HP.required true
            , HE.onValueChange $ HE.input action
            ] <> optionalValue
        , if hasError
              then HH.ul_ $ map (\e -> HH.li_ [HH.text e]) errors
              else HH.text ""
        ]
  where
    hasError :: Boolean
    hasError =
        not $ Array.null errors

    errorClass :: forall r i_. Array (HP.IProp ( class :: String | r ) i_)
    errorClass =
        if hasError
            then [HP.class_ $ H.ClassName "error"]
            else []

    optionalValue :: forall r i_. Array (HP.IProp ( value :: String | r ) i_)
    optionalValue = case value of
        Nothing ->
            []
        Just val -> [ HP.value val ]

-- | Render a standard `HH.button` element.
button :: forall p i. String -> HP.ButtonType -> H.ClassName -> (Unit -> i Unit) -> HH.HTML p (i Unit)
button label type_ class_ action =
    HH.button
        [ HE.onClick $ HE.input_ action, HP.class_ class_, HP.type_ type_ ]
        [ HH.text label ]

-- | Render a button to submit it's form. Uses the primary button styling.
submitButton :: forall p i. String -> HH.HTML p i
submitButton label =
    HH.button
        [ HP.class_ (H.ClassName "primary"), HP.type_ HP.ButtonSubmit ]
        [ HH.text label ]
