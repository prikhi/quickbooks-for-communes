{- | The page for editing a QuickBooks Company. Presents a form for select the
   | Company's TripAdvances account & adding A/P Store Accounts.


    TODO:
        * Handle loading & no companies returned
        * Handle loading & no accounts for company
-}
module Pages.EditCompany
    ( component
    , Query
    ) where

import Prelude

import Control.Monad.State (class MonadState)
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse, for_)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as E

import AccountSelect as AccountSelect
import App
    ( class SelectComponent, class PreventDefaultEnter
    , class PreventDefaultSubmit, preventSubmit
    )
import Forms (labelWrapper, optionalInput, button, submitButton, formErrors)
import Server
    ( CompanyData(..), CompanyAccounts(..), AccountData(..), StoreAccount(..)
    , EditCompanyData(..)
    , class Server, companiesRequest, companyAccountsRequest, accountsRequest
    , editCompanyRequest
    )
import Validation as V


component :: forall m q i o
    . Server m
   => SelectComponent m
   => PreventDefaultEnter m
   => PreventDefaultSubmit m
   => H.Component HH.HTML q i o m
component = H.mkComponent
    { initialState: const initial
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

type State =
    { companies :: Array CompanyData
    , company :: Maybe String
    , accounts :: Array AccountData
    , tripAdvances :: Maybe AccountData
    , storeAccounts :: Array (Tuple StoreAccountCount StoreAccount)
    , storeAccountCounter :: StoreAccountCount
    , errors :: V.FormErrors
    , submitSuccess :: Boolean
    }

initial :: State
initial =
    { companies: []
    , company: Nothing
    , accounts: []
    , tripAdvances: Nothing
    , storeAccounts: [ Tuple (StoreAccountCount 0) $ StoreAccount { name: "", account: 0 } ]
    , storeAccountCounter: StoreAccountCount 1
    , errors: V.empty
    , submitSuccess: false
    }

-- TODO: Validate StoreAccount Name & Account uniqueness
validate :: V.Validator State EditCompanyData
validate st = V.toEither <<< map EditCompany $
    { tripAdvances: map (\(AccountData a) -> a.id) st.tripAdvances, storeAccounts: _ }
        <$> (Array.catMaybes <$> traverseWithIndex validateStoreAccount st.storeAccounts)
  where
    validateStoreAccount :: Int -> V.Validation (Tuple StoreAccountCount StoreAccount) (Maybe StoreAccount)
    validateStoreAccount index (Tuple _ (StoreAccount sa)) =
        let fieldPrefix = "store-account-" <> show index <> "-" in
        if sa.name /= "" || sa.account > 0 then
            map (Just <<< StoreAccount) $ { name: _, account: _ }
                <$> V.validateNonEmpty (fieldPrefix <> "name") (Just sa.name)
                <*> validateAccountId (fieldPrefix <> "account") sa.account
        else
            pure Nothing
    validateAccountId :: String -> V.Validation Int Int
    validateAccountId field value =
        if Array.findIndex (\(AccountData a) -> a.id == value) st.accounts == Nothing then
            V.singleError field "An Account is required."
        else
            pure value


-- | An incrementing counter for each StoreAccount, used for the StoreAccount's
-- | AccountSelect child component.
data StoreAccountCount = StoreAccountCount Int
derive instance storeAccountCountGeneric :: Generic StoreAccountCount _
derive instance storeAccountCountEq :: Eq StoreAccountCount
derive instance storeAccountCountOrd :: Ord StoreAccountCount
instance storeAccountCountShow :: Show StoreAccountCount where
    show = genericShow

-- | Get the next StoreAccountCount & increment the counter in the State.
nextStoreAccountCount :: forall m. MonadState State m => m StoreAccountCount
nextStoreAccountCount = do
    counter <- H.gets _.storeAccountCounter
    H.modify_ \st -> st
        { storeAccountCounter =
            (\(StoreAccountCount c) -> StoreAccountCount $ c + 1) st.storeAccountCounter
        }
    pure counter


type ChildSlots =
    ( tripAdvancesSelect :: AccountSelect.Slot Unit
    , storeAccountSelect :: AccountSelect.Slot StoreAccountCount
    )

_tripAdvancesSelect = SProxy :: SProxy "tripAdvancesSelect"

_storeAccountSelect = SProxy :: SProxy "storeAccountSelect"


type Query = Const Void

data Action
    = Initialize
    | InputCompany String
    | HandleTripAdvanceSelect AccountSelect.Message
    | InputStoreAccountName Int String
    | HandleStoreAccountSelect Int AccountSelect.Message
    | DeleteStoreAccount Int
    | AddStoreAccount
    | SubmitForm E.Event

handleAction :: forall m o
    . Server m
   => PreventDefaultSubmit m
   => Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
    Initialize ->
        -- Fetch the Companies, Accounts, StoreAccounts, & TripAdvances Account
        companiesRequest >>= case _ of
            Right cs -> do
                H.modify_ (_ { companies = cs })
                case Array.head cs of
                    Just (CompanyData company) -> do
                        H.modify_ (_ { company = Just $ show company.id })
                        fetchCompanyData company.id
                    Nothing ->
                        pure unit
            Left _ ->
                pure unit
    InputCompany str -> do
        H.modify_ (_ { company = Just str })
        case Int.fromString str of
            Nothing ->
                pure unit
            Just companyId ->
                fetchCompanyData companyId
    HandleTripAdvanceSelect msg -> case msg of
        AccountSelect.Selected acc ->
            H.modify_ (_ { tripAdvances = Just acc })
        AccountSelect.Cleared ->
            H.modify_ (_ { tripAdvances = Nothing })
        AccountSelect.EnterKeyDownWhileClosed _ ->
            pure unit
    InputStoreAccountName index str -> do
        updateStoreAccount index \(StoreAccount sa) ->
            StoreAccount $ sa { name = str }
    HandleStoreAccountSelect index msg -> case msg of
        AccountSelect.Selected (AccountData acc) ->
            updateStoreAccount index \(StoreAccount sa) ->
                StoreAccount $ sa { account = acc.id }
        AccountSelect.Cleared ->
            updateStoreAccount index \(StoreAccount sa) ->
                StoreAccount $ sa { account = 0 }
        AccountSelect.EnterKeyDownWhileClosed _ ->
            pure unit
    DeleteStoreAccount index ->
        H.modify_ \st -> st
            { storeAccounts = fromMaybe st.storeAccounts
                $ Array.deleteAt index st.storeAccounts
            }
    AddStoreAccount ->
        addStoreAccount
    SubmitForm ev -> do
        -- Validate & submit the form, setting the errors or success status.
        preventSubmit ev
        st <- H.get
        case st.company >>= Int.fromString of
            Nothing ->
                H.modify_ (_ { errors = V.singleton "company" "Select a Company." })
            Just companyId ->
                case validate st of
                    Left errs ->
                        H.modify_ (_ { errors = errs })
                    Right ecd -> do
                        H.modify_ (_ { errors = V.empty })
                        editCompanyRequest companyId ecd >>= \r -> case r.body of
                            Left errs ->
                                H.modify_ (_ { errors = errs })
                            Right _ ->
                                H.modify_ (_ { submitSuccess = true })
  where
    -- | Fetch a Company's Accounts & CompanyAccounts.
    fetchCompanyData :: forall a o_ m_. Server m_ => Int -> H.HalogenM State a ChildSlots o_ m_ Unit
    fetchCompanyData companyId = do
        accountsRequest companyId >>= case _ of
            Left _ ->
                pure unit
            Right accs ->
                H.modify_ (_ { accounts = accs })
        companyAccountsRequest companyId >>= case _ of
            Left _ ->
                pure unit
            Right (CompanyAccounts companyAccs) -> do
                storeAccounts <- pairStoreAccounts companyAccs.storeAccounts
                H.modify_ (_
                    { tripAdvances = companyAccs.tripAdvances
                    , storeAccounts = storeAccounts
                    })
                for_ storeAccounts \(Tuple storeCount (StoreAccount sa)) ->
                    H.query _storeAccountSelect storeCount
                        $ AccountSelect.Select sa.account unit
                for_ companyAccs.tripAdvances \(AccountData acc) ->
                    H.query _tripAdvancesSelect unit
                        $ AccountSelect.Select acc.id unit
                when (Array.null storeAccounts) addStoreAccount
    -- | Add a blank StoreAccount to the State.
    addStoreAccount :: forall m_
        . MonadState State m_
       => m_ Unit
    addStoreAccount = do
        nextSACount <- nextStoreAccountCount
        let blankStoreAccount = Tuple nextSACount
                $ StoreAccount { name: "", account: 0 }
        H.modify_ \st -> st
            { storeAccounts = st.storeAccounts <> [ blankStoreAccount ]
            }
    -- | Add StoreAccountCounts to an array of StoreAccounts.
    pairStoreAccounts :: forall m_
        . MonadState State m_
       => Array StoreAccount -> m_ (Array (Tuple StoreAccountCount StoreAccount))
    pairStoreAccounts = traverse $ \sa -> do
        count <- nextStoreAccountCount
        pure $ Tuple count sa
    -- | Update the StoreAccount at the given index.
    updateStoreAccount :: forall m_
        . MonadState State m_
       => Int -> (StoreAccount -> StoreAccount) -> m_ Unit
    updateStoreAccount index updater =
        H.modify_ \st -> st
            { storeAccounts =
                fromMaybe st.storeAccounts $
                    Array.modifyAt index (\(Tuple c sa) -> Tuple c $ updater sa)
                        st.storeAccounts
            }



-- | Render the form or a success message.
render :: forall m
    . SelectComponent m
   => PreventDefaultEnter m
   => State -> H.ComponentHTML Action ChildSlots m
render st =
    if st.submitSuccess then
        HH.p [HP.class_ $ H.ClassName "success"]
            [ HH.text "The company was successfully updated."
            ]
    else
        HH.form [ HE.onSubmit $ Just <<< SubmitForm, HP.class_ $ H.ClassName "edit-company" ]
            [ formErrors $ V.getFormErrors st.errors
            , companySelect st.errors st.company st.companies
            , tripAdvancesSelect st.errors st.accounts
            , renderStoreAccounts st
            , submitButton "Update Company"
            ]

-- | Render the fieldset for StoreAccounts.
renderStoreAccounts :: forall m
    . SelectComponent m
   => PreventDefaultEnter m
   => State -> H.ComponentHTML Action ChildSlots m
renderStoreAccounts st =
    HH.fieldset [ HP.class_ $ H.ClassName "store-accounts" ] $
        [ HH.legend_ [ HH.text "Store Accounts" ] ]
        <> Array.mapWithIndex (renderStoreAccountForm st.errors st.accounts) st.storeAccounts
        <> [ button "Add Store Account" HP.ButtonButton (H.ClassName "primary") AddStoreAccount
           ]

-- | Render the inputs for a single StoreAccount.
renderStoreAccountForm :: forall m
    . SelectComponent m
   => PreventDefaultEnter m
   => V.FormErrors
   -> Array AccountData
   -> Int
   -> Tuple StoreAccountCount StoreAccount
   -> H.ComponentHTML Action ChildSlots m
renderStoreAccountForm errors accounts index (Tuple counter (StoreAccount storeAccount)) =
    HH.div_
        [ optionalInput "Name" HP.InputText (Just storeAccount.name) (InputStoreAccountName index)
            (error "name") "The name that Communards will see."
        , labelWrapper "Account" (error "account") "The Store's A/P Account"
            $ HH.slot _storeAccountSelect counter AccountSelect.component accounts
            $ Just <<< HandleStoreAccountSelect index
        , button "Delete Store Account" HP.ButtonButton (H.ClassName "danger")
            $ DeleteStoreAccount index
        , HH.hr_
        ]
  where
    error f =
        V.getFieldErrors ("store-account-" <> show index <> "-" <> f) errors


-- | Render the Select element for the Companies.
companySelect :: forall q. V.FormErrors -> Maybe String -> Array CompanyData -> HH.HTML q Action
companySelect errors selected companies =
    labelWrapper "Company" (V.getFieldErrors "company" errors) "The company to edit."
        $ HH.select
            [ HP.required true
            , HP.autofocus true
            , HE.onValueChange $ Just <<< InputCompany
            ]
        $ map renderCompany companies
  where
    renderCompany :: forall a. CompanyData -> HH.HTML q a
    renderCompany (CompanyData company) =
        HH.option
            [ HP.value $ show company.id
            , HP.selected $ selected == Just (show company.id)
            ]
            [ HH.text company.name ]

-- | Render the AccountSelect component for the TripAdvances field.
tripAdvancesSelect :: forall m
    . SelectComponent m
   => PreventDefaultEnter m
   => V.FormErrors -> Array AccountData -> H.ComponentHTML Action ChildSlots m
tripAdvancesSelect errors accounts =
    labelWrapper "Trip Advances" (V.getFieldErrors "trip-advances" errors) "The account to credit for Trip Entries."
        $ HH.slot _tripAdvancesSelect unit AccountSelect.component accounts
        $ Just <<< HandleTripAdvanceSelect
