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
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (Map)
import Data.Map as M
import Data.Validation.Semigroup as V
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as E

import App
    ( class PreventDefaultSubmit, preventSubmit
    , class LogToConsole, logShow
    )
import Server (NewCompanyData(..))


component :: forall m i o
    . PreventDefaultSubmit m
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
    { name :: Maybe String
    , username :: Maybe String
    , password :: Maybe String
    , errors :: FormErrors
    }

-- | Start the form as blank.
initial :: State
initial =
    { name: Nothing
    , username: Nothing
    , password: Nothing
    , errors: M.empty
    }

validate :: State -> Either FormErrors NewCompanyData
validate st = V.toEither <<< map NewCompany $
        { name: _, user: _, password: _ }
            <$> validateNonEmpty "name" st.name
            <*> validateNonEmpty "username" st.username
            <*> validateNonEmpty "password" st.password


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
        H.modify_ (_ { name = Just str })
        revalidate_ "name"
        pure next
    InputUser str next -> do
        H.modify_ (_ { username = Just str })
        revalidate_ "username"
        pure next
    InputPass str next -> do
        H.modify_ (_ { password = Just str })
        revalidate_ "password"
        pure next
    SubmitForm ev next -> do
        preventSubmit ev
        st <- H.get
        logShow st
        case validate st of
            Left errs ->
                H.modify_ (_ { errors = errs })
            Right ncd -> do
                H.modify_ (_ { errors = empty })
        pure next
  where
    revalidate_ = revalidate validate


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
            [ input "Company Name" HP.InputText st.name InputName $ errors "name"
            , input "Username" HP.InputText st.username InputUser $ errors "username"
            , input "Password" HP.InputPassword st.password InputPass $ errors "password"
            , submitButton "Add Company"
            ]
        ]
  where
    liText t = HH.li_ [ HH.text t ]
    errors field = getFieldErrors field st.errors



-- Forms - TODO: stick in module

-- | Render a standard `HH.input` element with a label and error list.
input :: forall p i. String -> HP.InputType -> Maybe String -> (String -> Unit -> i Unit) -> Array String -> HH.HTML p (i Unit)
input label type_ value action errors =
    HH.label_
        [ HH.div_ [ HH.text label ]
        , HH.input $
            [ HP.type_ type_
            , HP.required true
            , HE.onValueChange $ HE.input action
            ] <> optionalValue <> errorClass
        , if hasError
              then HH.ul [] $ map (\e -> HH.li_ [HH.text e]) errors
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



-- Validation - TODO: stick in module

-- | Each Field can have multiple error message.
type FormErrors = Map String (Array String)

-- | A record is WithErrors if it contains FormErrors in it's `errors` field.
type WithErrors s = { errors :: FormErrors | s }

-- | Validation results in some errors or the validated type.
type ValidationResult a = Either FormErrors a

-- | A validator transforms an input type into the resuls of a validation.
type Validator a b = (->) a (ValidationResult b)


-- | Empty FormErrors means no errors!
empty :: FormErrors
empty = M.empty

-- | Get the errors for a field.
getFieldErrors :: String -> FormErrors -> Array String
getFieldErrors field =
    M.lookup field >>> fromMaybe []

-- | Return a single error from a field validation.
singleError :: forall a. String -> String -> V.V FormErrors a
singleError field message =
    V.invalid $ M.singleton field [message]


-- | Ensure a Maybe String has a non-empty value present.
validateNonEmpty :: String -> Maybe String -> V.V FormErrors String
validateNonEmpty field = case _ of
    Nothing -> singleError field "A value is required."
    Just "" -> singleError field "A value is required."
    Just s  -> pure s

-- | If the form was invalid, recheck the validation & update the field's
-- | error status.
revalidate :: forall m a b
    . MonadState (WithErrors a) m
   => Validator (WithErrors a) b -> String -> m Unit
revalidate validator field = do
    st <- H.get
    let newErrors = case validator st of
            Right _ ->
                (_ { errors = empty })
            Left errs -> \s ->
                if not $ M.isEmpty s.errors
                then case getFieldErrors field errs of
                    [] ->
                        s { errors = M.delete field s.errors }
                    _ ->
                        s { errors = M.unionWith mergeUnique s.errors errs }
                else
                    s
    H.modify_ newErrors

-- | Merge the arrays, ensuring all resulting elements are unique.
mergeUnique :: forall a. Ord a => Array a -> Array a -> Array a
mergeUnique arr1 arr2 = Array.nub $ arr1 <> arr2
