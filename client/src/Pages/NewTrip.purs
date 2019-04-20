{- | The page component for adding a new Trip Entry.

TODO:
    * Handle waiting for companies to load & no companies returned
    * Handle unselected company, waiting for accounts to load & no accounts
      returned
    * Refactor enter keydown filtering from App module to new property
      function? I.e., filter the event when passing it to onKeyDown instead of
      in preventEnter? Or both?
    * Transaction tables
    * Store Credit fieldsets
    * Form validation
    * Form submission
    * Custom styling for checkboxes
    * Use custom date picker? https://github.com/slamdata/purescript-halogen-datepicker
-}
module Pages.NewTrip
    ( component
    , Query
    ) where

import Prelude

import Control.Monad.State (class MonadState)
import Data.Array as Array
import Data.Const (Const)
import Data.Date (Date, year, month, day)
import Data.DateTime (date)
import Data.Decimal as Decimal
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Foldable (fold)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence, for_)
import Data.Tuple (Tuple(..))
import DOM.HTML.Indexed (HTMLinput)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as E
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.KeyboardEvent as KE

import AccountSelect as AccountSelect
import App
    ( class PreventDefaultSubmit, preventSubmit
    , class PreventDefaultEnter, preventEnter
    , class PreventDefaultClick, preventClick
    , class LogToConsole, logShow
    , class DateTime, parseDate, now
    , class FocusElement
    , class SelectComponent
    )
import Forms
    ( input, dateInput, dollarInput, submitButton, labelWrapper, optionalValue
    , button
    )
import Server
    ( CompanyData(..), AccountData
    , class Server, companiesRequest, accountsRequest
    )
import Validation as V


-- | The "Add a Trip" Page.
component :: forall q i o m
    . LogToConsole m
   => PreventDefaultSubmit m
   => PreventDefaultEnter m
   => PreventDefaultClick m
   => DateTime m
   => Server m
   => FocusElement m
   => SelectComponent m
   => H.Component HH.HTML q i o m
component =
    H.mkComponent
        { initialState: const initial
        , render
        , eval: H.mkEval $ H.defaultEval
            { handleAction = eval
            , initialize = Just Initialize
            }
        }


type State =
    { companies :: Array CompanyData
    , accounts :: Array AccountData
    , company :: Maybe String
    , date :: Maybe String
    , name :: Maybe String
    , tripNumber :: Maybe String
    , cashAdvance :: Maybe String
    , cashReturned :: Maybe String
    , stops :: Array TripStop
    , errors :: V.FormErrors
    }

initial :: State
initial =
    { companies: []
    , accounts: []
    , company: Nothing
    , date: Nothing
    , name: Nothing
    , tripNumber: Nothing
    , cashAdvance: Nothing
    , cashReturned: Nothing
    , stops: [ initialStop ]
    , errors: V.empty
    }

type TripStop =
    { name :: Maybe String
    , stopTotal :: Maybe String
    , transactions :: Array Transaction
    }

initialStop :: TripStop
initialStop =
    { name: Nothing
    , stopTotal: Nothing
    , transactions: Array.replicate 3 initialTransaction
    }

type Transaction =
    { account :: Maybe AccountData
    , memo :: Maybe String
    , amount :: Maybe String
    , tax :: Maybe String
    , total :: Maybe String
    , isReturn :: Boolean
    }

initialTransaction :: Transaction
initialTransaction =
    { account: Nothing
    , memo: Nothing
    , amount: Nothing
    , tax: Just "5.3"
    , total: Nothing
    , isReturn: false
    }

-- TODO: These Ints should not be based on indexes or deleting the first or
-- middle rows does nothing! Use an increasing counter in the State & TripStop
-- types, pull value & bump it when adding new rows.
type ChildSlots =
    ( accountSelect :: AccountSelect.Slot (Tuple Int Int)
    )

_accountSelect = SProxy :: SProxy "accountSelect"


type Query = Const Void

-- TODO: Break out into TripStopAction & TripTransactionAction types?
--       That might mean separate components for TripStop & Transactions...
data Action
    = Initialize
    -- ^ Get the current date & set the Date & Trip Number fields.
    | InputCompany String
    | InputDate String
    | InputName String
    | InputNumber String
    | InputAdvance String
    | InputReturned String
    | RemoveStop Int
    | AddStop
    | StopInputName Int String
    | StopInputTotal Int String
    | StopAddRows Int ME.MouseEvent
    | TransactionHandleAccount Int Int AccountSelect.Message
    | TransactionInputMemo Int Int String
    | TransactionInputAmount Int Int String
    | TransactionInputTax Int Int String
    | TransactionInputTotal Int Int String
    | TransactionCheckReturn Int Int Boolean
    | TransactionClickRemove Int Int ME.MouseEvent
    | TransactionInputEnter Int Int KE.KeyboardEvent
    | SubmitForm E.Event
    -- ^ Validate & POST the form.

-- | Initialize, update, & submit the form.
eval :: forall m o
    . LogToConsole m
   => PreventDefaultSubmit m
   => PreventDefaultEnter m
   => PreventDefaultClick m
   => DateTime m
   => Server m
   => FocusElement m
   => Action -> H.HalogenM State Action ChildSlots o m Unit
eval = case _ of
    Initialize -> do
        currentDate <- date <$> now
        H.modify_ (_ { date = Just $ makeDate currentDate })
        updateTripNumber currentDate
        companiesRequest >>= case _ of
            Right cs -> do
                H.modify_ (_ { companies = cs })
                case Array.head cs of
                    Just (CompanyData company) -> do
                        H.modify_ (_ { company = Just $ show company.id })
                        fetchAccounts company.id
                    Nothing ->
                        pure unit
            Left _ ->
                pure unit
    InputCompany str -> do
        H.modify_ (_ { company = Just str })
        case Int.fromString str of
            Just companyId ->
                fetchAccounts companyId
            Nothing ->
                pure unit
    InputDate str -> do
        autofillTripNumber str
        H.modify_ (_ { date = Just str })
    InputName str -> do
        H.modify_ (_ { name = Just str })
    InputNumber str -> do
        H.modify_ (_ { tripNumber = Just str })
    InputAdvance str -> do
        H.modify_ (_ { cashAdvance = Just str })
    InputReturned str -> do
        H.modify_ (_ { cashReturned = Just str })
    RemoveStop index -> do
        deleteStop index
    AddStop -> do
        H.modify_ $ \st -> st { stops = st.stops <> [ initialStop ] }
    StopInputName index str -> do
        updateStop index (_ { name = Just str })
    StopInputTotal index str -> do
        updateStop index (_ { stopTotal = Just str })
    StopAddRows index ev -> do
        addRowsToStop index 3
        preventClick ev
    TransactionHandleAccount stopIndex transIndex msg -> case msg of
        AccountSelect.Selected acc ->
            updateTransaction stopIndex transIndex (_ { account = Just acc })
        AccountSelect.Cleared ->
            updateTransaction stopIndex transIndex (_ { account = Nothing })
        AccountSelect.EnterKeyDownWhileClosed event ->
            preventEnter event *> focusNextRow stopIndex transIndex
    TransactionInputMemo stopIndex transIndex str -> do
        updateTransaction stopIndex transIndex (_ { memo = Just str })
    TransactionInputAmount stopIndex transIndex str -> do
        updateTransaction stopIndex transIndex (_ { amount = Just str })
        recalculateTransactionTotal stopIndex transIndex
    TransactionInputTax stopIndex transIndex str -> do
        updateTransaction stopIndex transIndex (_ { tax = Just str })
        recalculateTransactionTotal stopIndex transIndex
    TransactionInputTotal stopIndex transIndex str -> do
        updateTransaction stopIndex transIndex (_ { total = Just str })
        clearAmountAndTax stopIndex transIndex
    TransactionCheckReturn stopIndex transIndex val -> do
        updateTransaction stopIndex transIndex (_ { isReturn = val })
    TransactionClickRemove stopIndex transIndex ev -> do
        deleteTransaction stopIndex transIndex
        preventClick ev
    TransactionInputEnter stopIndex transIndex ev -> do
        wasPrevented <- preventEnter ev
        when wasPrevented
            $ focusNextRow stopIndex transIndex
    SubmitForm ev -> do
        st <- H.get
        logShow st
        preventSubmit ev
  where
    -- Build the string for a date input element.
    makeDate :: Date -> String
    makeDate date =
        fold
            [ show $ fromEnum $ year date
            , "-"
            , padZero $ fromEnum $ month date
            , "-"
            , padZero $ fromEnum $ day date
            ]
    -- Fetch the accounts for a company & update the state.
    fetchAccounts :: forall m_
        . MonadState State m_
       => Server m_
       => Int -> m_ Unit
    fetchAccounts companyId = do
        accountsRequest companyId >>= case _ of
            Right accs ->
                H.modify_ (_ { accounts = accs })
            Left _ ->
                pure unit
    -- Set the trip number given a new date input string, if the old number
    -- matches the previous date string, or the trip number is blank.
    autofillTripNumber :: forall m_
        . MonadState State m_
       => DateTime m_
       => String -> m_ Unit
    autofillTripNumber str = do
        oldDateString <- _.date <$> H.get
        oldDate <- map join <<< sequence $ parseDate <$> oldDateString
        mDate <- parseDate str
        number <- _.tripNumber <$> H.get
        case Tuple oldDate mDate of
            Tuple (Just old) (Just new) ->
                when (Just (tripNumberFromDate old) == number) $
                    updateTripNumber new
            Tuple Nothing (Just new) ->
                when (number == Nothing || number == Just "") $
                    updateTripNumber new
            Tuple _ Nothing ->
                pure unit
    -- Update the trip number given a new Date.
    updateTripNumber :: forall m_. MonadState State m_ => Date -> m_ Unit
    updateTripNumber date =
        H.modify_ (_ { tripNumber = Just $ tripNumberFromDate date })
    -- Build a trip number from a Date.
    tripNumberFromDate :: Date -> String
    tripNumberFromDate date =
        fold
            [ padZero $ fromEnum $ month date
            , padZero $ fromEnum $ day date
            , show $ fromEnum $ year date
            ]
    -- Ensure the Int is 2 characters long by padding it with a zero.
    padZero :: Int -> String
    padZero int =
        if int < 10 then
            "0" <> show int
        else show int
    -- Update the TripStop at the given index.
    updateStop :: forall m_
        . MonadState State m_
       => Int -> (TripStop -> TripStop) -> m_ Unit
    updateStop index updater =
        H.modify_ $ \st ->
            st { stops =
                fromMaybe st.stops $ Array.modifyAt index updater st.stops
            }
    -- Remove the TripStop at the given index.
    deleteStop :: forall m_. MonadState State m_ => Int -> m_ Unit
    deleteStop index =
        H.modify_ $ \st ->
            st { stops =
                fromMaybe st.stops $ Array.deleteAt index st.stops
            }
    -- Add additional transactions to a TripStop.
    addRowsToStop :: forall m_. MonadState State m_ => Int -> Int -> m_ Unit
    addRowsToStop index count =
        updateStop index $ \stop -> stop {
            transactions =
                stop.transactions <> Array.replicate count initialTransaction
            }
    -- Update the Transaction for a TripStop at the given indexes.
    updateTransaction :: forall m_
        . MonadState State m_
       => Int -> Int -> (Transaction -> Transaction) -> m_ Unit
    updateTransaction stopIndex transIndex updater =
        updateStop stopIndex $ \stop ->
            stop { transactions =
                fromMaybe stop.transactions
                    $ Array.modifyAt transIndex updater stop.transactions
            }
    -- Delete the Transaction for a TripStop at the given indexes.
    deleteTransaction :: forall m_
        . MonadState State m_
       => Int -> Int -> m_ Unit
    deleteTransaction stopIndex transIndex =
        updateStop stopIndex $ \stop ->
            stop { transactions =
                 fromMaybe stop.transactions
                    $ Array.deleteAt transIndex stop.transactions
            }
    -- Set the Total Amount for a Transaction if it's Amount is entered.
    recalculateTransactionTotal :: forall m_
        . MonadState State m_
       => Int -> Int -> m_ Unit
    recalculateTransactionTotal stopIndex transIndex =
        updateTransaction stopIndex transIndex $ \transaction ->
            transaction { total = map (Decimal.toFixed 2) $ calculateTotal transaction }
    -- Calculate the total from the Tax & Amount, Defaulting the Tax to 0% if
    -- not present.
    calculateTotal :: Transaction -> Maybe Decimal.Decimal
    calculateTotal transaction =
        let taxMultiplier =
                fromMaybe (Decimal.fromInt 1)
                $ map (\percent -> Decimal.fromInt 1 + percent / Decimal.fromInt 100)
                    $ toDecimal transaction.tax
        in (*) <$> toDecimal transaction.amount <*> pure taxMultiplier
    -- Clear the Amount & Tax % fields for a TripStop's Transaction.
    clearAmountAndTax :: forall m_. MonadState State m_ => Int -> Int -> m_ Unit
    clearAmountAndTax stopIndex transIndex =
        updateTransaction stopIndex transIndex
            (_ { amount = Nothing, tax = Nothing })
    -- Focus the transaction row after the given one, adding a new row if one
    -- doesn't exist.
    focusNextRow :: forall f o_ m_
        . FocusElement m_
       => Int -> Int -> H.HalogenM State f ChildSlots o_ m_ Unit
    focusNextRow stopIndex transactionIndex = do
        st <- H.get
        for_ (Array.index st.stops stopIndex) \stop -> do
            let transactionCount = Array.length stop.transactions
            when (transactionCount - 1 <= transactionIndex)
                $ addRowsToStop stopIndex 1
            focusStopTransaction stopIndex (transactionIndex + 1)
    -- Change focus to the first input of a TripStop's Transaction.
    focusStopTransaction :: forall s f o_ m_
        . FocusElement m_
       => Int -> Int -> H.HalogenM s f ChildSlots o_ m_ Unit
    focusStopTransaction stopIndex transIndex =
        void $ H.query _accountSelect (Tuple stopIndex transIndex)
            $ AccountSelect.Focus unit


-- | Render the Add a Trip form.
render :: forall m
    . SelectComponent m
   => PreventDefaultEnter m
   => State -> H.ComponentHTML Action ChildSlots m
render st =
    HH.form [ HE.onSubmit $ Just <<< SubmitForm ] $
        [ companySelect st.company st.companies
        , dateInput "Date" st.date InputDate (errors "date")
            "The day you went on the trip."
        , input "Tripper" HP.InputText st.name InputName (errors "name")
            "Your name."
        , input "Trip Number" HP.InputText st.tripNumber InputNumber (errors "number")
            "The number given when taking out an advance."
        , dollarInput "Trip Advance" st.cashAdvance InputAdvance (errors "advance")
            "The amount of cash taken out for the trip."
        , dollarInput "Cash Returned" st.cashReturned InputReturned (errors "return")
            "The amount of cash remaining after your trip."
        , cashSpentInput st
        ]
        <> Array.mapWithIndex (renderTripStop st.accounts st.errors) st.stops
        <> [ button "Add Stop" HP.ButtonButton (H.ClassName "primary") AddStop
           , submitButton "Submit Trip"
           ]
  where
    errors :: String -> Array String
    errors field = V.getFieldErrors field st.errors


-- | Render the fieldset for a TripStop.
renderTripStop :: forall m
    . SelectComponent m
   => PreventDefaultEnter m
   => Array AccountData
   -> V.FormErrors
   -> Int
   -> TripStop
   -> H.ComponentHTML Action ChildSlots m
renderTripStop accounts formErrors stopIndex tripStop =
    HH.fieldset_
        [ HH.legend_ [ HH.text stopLabel ]
        , input "Stop Name" HP.InputText tripStop.name (StopInputName stopIndex) (errors "name")
            "The name of the store."
        , dollarInput "Total Spent" tripStop.stopTotal (StopInputTotal stopIndex) (errors "total")
            "The amount spent at the store."
        , renderTransactionTable accounts formErrors stopIndex tripStop.stopTotal tripStop.transactions
        , button "Remove Stop" HP.ButtonButton (H.ClassName "danger") (RemoveStop stopIndex)
        ]
  where
    errors :: String -> Array String
    errors field =
        V.getFieldErrors ("trip-stop-" <> show stopIndex <> "-" <> field)
            formErrors
    stopLabel = case tripStop.name of
        Nothing ->
            "Trip Stop"
        Just "" ->
            "Trip Stop"
        Just str ->
            "Trip Stop: " <> str

renderTransactionTable :: forall m
    . SelectComponent m
   => PreventDefaultEnter m
   => Array AccountData
   -> V.FormErrors
   -> Int
   -> Maybe String
   -> Array Transaction
   -> H.ComponentHTML Action ChildSlots m
renderTransactionTable accounts formErrors stopIndex stopTotal transactions =
  let
    transactionTotal =
        Array.foldl sumTotals (Decimal.fromInt 0) transactions
    outOfBalance =
        fromMaybe transactionTotal
            $ (-) <$> toDecimal stopTotal <*> pure transactionTotal
  in
    HH.table_
        [ HH.thead_
            [ HH.th_ [ HH.text "Account" ]
            , HH.th_ [ HH.text "Details" ]
            , HH.th_ [ HH.text "Cost" ]
            , HH.th_ [ HH.text "Tax %" ]
            , HH.th_ [ HH.text "Total Cost" ]
            , HH.th_ [ HH.text "Return?" ]
            , HH.th_ []
            ]
        , HH.tbody_
            $ Array.mapWithIndex (renderTransaction accounts formErrors stopIndex)
                transactions
        , HH.tfoot_
            [ HH.tr_ [ addRowsCell ]
            , HH.tr_
                [ HH.th [ HP.colSpan 4 ] [ HH.text "Total:"]
                , HH.td [ HP.colSpan 3 ] [ HH.text $ "$" <> Decimal.toFixed 2 transactionTotal]
                ]
            -- TODO: Color amount when out of balance
            , HH.tr_
                [ HH.th [ HP.colSpan 4 ] [ HH.text "Out of Balance:" ]
                , HH.td [ HP.colSpan 3 ] [ HH.text $ "$" <> Decimal.toFixed 2 outOfBalance ]
                ]
            ]
        ]
  where
    addRowsCell :: forall p. HH.HTML p Action
    addRowsCell =
        HH.td [ HP.colSpan 7 ]
            [ HH.small_
                [ HH.a
                    [ HP.href "#"
                    , HP.title "Add Rows"
                    , HE.onClick $ Just <<< StopAddRows stopIndex
                    ]
                    [ HH.text "Add Rows" ]
                ]
            ]
    sumTotals :: Decimal.Decimal -> Transaction -> Decimal.Decimal
    sumTotals acc transaction =
        let multiplier = Decimal.fromInt $
                if transaction.isReturn
                    then -1
                    else 1
        in
            fromMaybe acc
                $ (+) <$> pure acc <*> map (\d -> multiplier * d) (toDecimal transaction.total)


-- | Render the form row for a
-- | TODO: Highlight errors & render another row below w/ messages.
renderTransaction :: forall m
    . SelectComponent m
   => PreventDefaultEnter m
   => Array AccountData
   -> V.FormErrors
   -> Int
   -> Int
   -> Transaction
   -> H.ComponentHTML Action ChildSlots m
renderTransaction accounts formErrors stopIndex transactionIndex transaction =
    HH.tr_ $ map centeredCell
        [ [ HH.slot _accountSelect (Tuple stopIndex transactionIndex)
            AccountSelect.component accounts
            $ Just <<< TransactionHandleAccount stopIndex transactionIndex
          ]
        , [ tableInput "memo" transaction.memo
            (mkAction TransactionInputMemo)
            (mkAction TransactionInputEnter)
            false
            [ HP.ref $ transactionDetailRef stopIndex transactionIndex ]
          ]
        , [ tableAmountInput "item-price" transaction.amount
                (mkAction TransactionInputAmount)
                (mkAction TransactionInputEnter)
                false
          ]
        , [ tableAmountInput "tax-rate" transaction.tax
                (mkAction TransactionInputTax)
                (mkAction TransactionInputEnter)
                false
          ]
        , [ tableAmountInput "item-total" transaction.total
                (mkAction TransactionInputTotal)
                (mkAction TransactionInputEnter)
                false
          ]
        , [ tableCheckbox "returned" transaction.isReturn
                (mkAction TransactionCheckReturn)
                (mkAction TransactionInputEnter)
          ]
        , [ HH.a
                [ HE.onClick $ Just <<< mkAction TransactionClickRemove
                , HP.href "#"
                , HP.title "Delete Row"
                , HP.class_ $ H.ClassName "danger"
                ]
                [ HH.i [ HP.class_ $ H.ClassName "fas fa-times" ] [] ]
          ]
        ]
  where
    mkAction :: forall a. (Int -> Int -> a) -> a
    mkAction action =
        action stopIndex transactionIndex
    centeredCell :: forall p i. Array (HH.HTML p i) -> HH.HTML p i
    centeredCell =
        HH.td [ HP.class_ $ H.ClassName "align-center" ]

transactionDetailRef :: Int -> Int -> H.RefLabel
transactionDetailRef stopI transI =
    H.RefLabel
        $ "__newtrip_transaction_detail_" <> show stopI <> "_" <> show transI


-- Inputs

tableInput :: forall p i
    . String
   -> Maybe String
   -> (String -> i)
   -> (KE.KeyboardEvent -> i)
   -> Boolean
   -> Array (HP.IProp HTMLinput i)
   -> HH.HTML p i
tableInput name value action enterKeyAction hasError attrs =
    HH.input $
        [ HP.type_ HP.InputText
        , HP.name name
        , HE.onValueInput $ Just <<< action
        , HE.onKeyDown $ Just <<< enterKeyAction
        ] <> optionalValue value <> errorClass hasError <> attrs

tableAmountInput :: forall p i
    . String
   -> Maybe String
   -> (String -> i)
   -> (KE.KeyboardEvent -> i)
   -> Boolean
   -> HH.HTML p i
tableAmountInput name value action enterKeyAction hasError =
    HH.input $
        [ HP.type_ HP.InputNumber
        , HP.name name
        , HE.onValueInput $ Just <<< action
        , HE.onKeyDown $ Just <<< enterKeyAction
        , HP.min 0.0
        , HP.step $ HP.Step 0.01
        ] <> optionalValue value <> errorClass hasError

tableCheckbox :: forall p i
    . String
   -> Boolean
   -> (Boolean -> i)
   -> (KE.KeyboardEvent -> i)
   -> HH.HTML p i
tableCheckbox name value action enterKeyAction =
    HH.input $
        [ HP.type_ HP.InputCheckbox
        , HP.name name
        , HE.onChecked $ Just <<< action
        , HE.onKeyDown $ Just <<< enterKeyAction
        , HP.checked value
        ]

errorClass :: forall r i. Boolean -> Array (HP.IProp ( class :: String | r ) i)
errorClass hasError =
    if hasError then
        [ HP.class_ $ H.ClassName "error" ]
    else
        []

-- | Render a disabled input for the Cash Spent, calculated from the Trip
-- | Advance and the Cash Returned.
cashSpentInput :: forall a q m. State -> H.ComponentHTML a q m
cashSpentInput st =
  let cashSpent = (-)
        <$> toDecimal st.cashAdvance
        <*> toDecimal st.cashReturned
   in
    labelWrapper "Cash Spent" [] "The amount of cash spent during your trip." $
        HH.input $
            [ HP.type_ HP.InputNumber
            , HP.disabled true
            ] <> optionalValue (Decimal.toFixed 2 <$> cashSpent)

-- | Render the select element for companies.
companySelect :: forall q m
    . Maybe String
   -> Array CompanyData
   -> H.ComponentHTML Action q m
companySelect selectedCompany companies =
    labelWrapper "Company" [] "The company to make a trip for."
        $ HH.select
            [ HP.required true
            , HP.autofocus true
            , HE.onValueChange $ Just <<< InputCompany
            ]
        $ map
            (\(CompanyData company) ->
                HH.option
                    [ HP.value $ show company.id
                    , HP.selected $ selectedCompany == Just (show company.id)
                    ]
                    [ HH.text company.name ]
            )
            companies


-- | Attempt to convert a potential string into a Decimal
toDecimal :: Maybe String -> Maybe Decimal.Decimal
toDecimal = join <<< map Decimal.fromString
