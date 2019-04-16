{- | The page for add a New QuickBooks Company. This presents a form for the
   | Company which, on submission, displays a download link for the QuickBooks
   | WebConnector's @qwc@ config file.

-}
module Pages.NewCompany
    ( component
    , Query
    ) where

import Prelude

import Control.Monad.State (class MonadState)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as E

import App
    ( class PreventDefaultSubmit, preventSubmit
    , class ManageObjectURLs, createObjectURL, revokeObjectURL
    )
import Forms (formErrors, input, submitButton)
import Server (class Server, newCompanyRequest, NewCompanyData(..), QWCFile(..))
import Validation as V


component :: forall m q i o
    . PreventDefaultSubmit m
   => Server m
   => ManageObjectURLs m
   => H.Component HH.HTML q i o m
component = H.mkComponent
    { initialState: const initial
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = eval
        , finalize = Just Destroy
        }
    }

-- | The form data.
type State =
    { name :: Maybe String
    , username :: Maybe String
    , password :: Maybe String
    , errors :: V.FormErrors
    , objectURL :: Maybe String
    }

-- | Start the form as blank.
initial :: State
initial =
    { name: Nothing
    , username: Nothing
    , password: Nothing
    , errors: V.empty
    , objectURL: Nothing
    }

validate :: State -> Either V.FormErrors NewCompanyData
validate st = V.toEither <<< map NewCompany $
        { name: _, user: _, password: _ }
            <$> V.validateNonEmpty "name" st.name
            <*> V.validateNonEmpty "username" st.username
            <*> V.validateNonEmpty "password" st.password


type Query = Const Void

-- | Form input & submission events.
data Action
    = InputName String
    | InputUser String
    | InputPass String
    | SubmitForm E.Event
    -- ^ Validate & Submit the Form to the Server.
    | Destroy
    -- ^ Revoke the objectURL.


-- | Update & submit the form.
eval :: forall m g o
    . PreventDefaultSubmit m
   => Server m
   => ManageObjectURLs m
   => Action -> H.HalogenM State Action g o m Unit
eval = case _ of
    InputName str -> do
        H.modify_ (_ { name = Just str })
        revalidate "name"
    InputUser str -> do
        H.modify_ (_ { username = Just str })
        revalidate "username"
    InputPass str -> do
        H.modify_ (_ { password = Just str })
        revalidate "password"
    SubmitForm ev -> do
        preventSubmit ev
        st <- H.get
        case validate st of
            Left errs ->
                H.modify_ (_ { errors = errs })
            Right ncd -> do
                H.modify_ (_ { errors = V.empty })
                newCompanyRequest ncd >>= \r -> case r.body of
                    Left errs ->
                        H.modify_ (_ { errors = errs })
                    Right (QWCFile fileBlob) -> do
                        qwcURL <- createObjectURL fileBlob
                        revoke
                        H.modify_ (_ { objectURL = Just qwcURL })
    Destroy ->
        revoke
  where
    -- | Re-validate the form & update the errors.
    revalidate :: forall n. MonadState State n => String -> n Unit
    revalidate = V.revalidate validate
    -- | Revoke the objectURL if one is stored.
    revoke :: forall n. MonadState State n => ManageObjectURLs n => n Unit
    revoke = H.get >>= _.objectURL >>> case _ of
        Nothing ->
            pure unit
        Just objectURL ->
            revokeObjectURL objectURL


-- | Render the New Company page.
render :: forall q. State -> HH.HTML q Action
render st =
    maybe showForm showDownload st.objectURL
  where
    showForm :: HH.HTML q Action
    showForm =
        HH.div_
            [ HH.p_
                [ HH.text "Use this form to add a new QuickBooks Company. A Company \
                    \will sync to a specific QuickBooks file, pulling it's account \
                    \data and allowing entries to be created for it."
                ]
            , HH.p_
                [ HH.text "After creating a Company, you will be given a configuration \
                    \file for the QuickBooks WebConnector. You will need to open the \
                    \company in QuickBooks, and add the file to the WebConnector to \
                    \initialize the data."
                ]
            , HH.form [ HE.onSubmit $ Just <<< SubmitForm ]
                [ formErrors $ V.getFormErrors st.errors
                , input "Company Name" HP.InputText st.name InputName (errors "name")
                    "For pages & forms"
                , input "Username" HP.InputText st.username InputUser (errors "username")
                    "Used with the web connector"
                , input "Password" HP.InputPassword st.password InputPass (errors "password")
                    "Used with the web connector"
                , submitButton "Add Company"
                ]
            ]
    showDownload :: String -> HH.HTML q Action
    showDownload objectURL =
        HH.div_
            [ HH.p [ HP.class_ $ H.ClassName "success" ]
                [ HH.text "Your company was successfully created." ]
            , HH.p_
                [ HH.text "The last step in setting up a new company is to manually \
                    \run the first sync manually. To do this, you need to:"
                ]
            , HH.ol_ $ map liText
                [ "Ensure the QuickBooks Web Connector is running."
                , "Un-check all the Auto-Run checkboxes."
                , "Open the Company's File in QuickBooks."
                , "Save your Company's Web Connector Configuration(qwc) \
                    \File using the button below."
                , "Open the file in QuickBooks Web Connector using the \"Add \
                    \an Application\" button."
                , "Authorize the application in the Web Connector popup."
                , "Authorze the application in QuickBooks popup. Select the \
                    \option to allow access even when QuickBooks is not running."
                , "Make sure only the new application is selected in the \
                    \Web Connector list."
                , "In the popup, enter the password you used when creating \
                    \the company. Choose to save the password when prompted."
                , "Click the \"Update Selected\" button in the Web Connector."
                , "Re-check any of the Auto-Run checkboxes you unchecked earlier."
                , "The new company should now be available when creating entries."
                ]
            , downloadButton (fromMaybe "sync" st.name <> ".qwc")
                "Download Your QWC File" objectURL
            ]
    liText t = HH.li_ [ HH.text t ]
    errors field = V.getFieldErrors field st.errors


-- | Show a download button using an `a` element with a `download` attribute.
downloadButton :: forall q
    . String
   -- ^ Filename
   -> String
   -- ^ Button Text
   -> String
   -- ^ Object URL
   -> HH.HTML q Action
downloadButton filename text objectURL =
    HH.a
        [ HP.href objectURL
        , HP.attr (H.AttrName "download") filename
        , HP.class_ $ H.ClassName "link-button"
        ]
        [ HH.text text ]
