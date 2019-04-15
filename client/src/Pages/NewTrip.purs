{- | The page component for adding a new Trip Entry.

TODO:
    * Handle waiting for companies to load & no companies returned
    * Handle unselected company, waiting for accounts to load & no accounts
      returned
    * Transaction tables
    * Form validation
    * Form submission
    * Use custom selects? https://citizennet.github.io/purescript-halogen-select/
    * Use custom date picker? https://github.com/slamdata/purescript-halogen-datepicker
-}
module Pages.NewTrip
    ( component
    , Query(..)
    ) where

import Prelude

import Control.Monad.State (class MonadState)
import Data.Array as Array
import Data.Date (Date, year, month, day)
import Data.DateTime (date)
import Data.Decimal as Decimal
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Foldable (fold)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence, traverse_, for_)
import Data.Tuple (Tuple(..))
import DOM.HTML.Indexed (HTMLinput)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as E
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.KeyboardEvent as KE

import App
    ( class PreventDefaultSubmit, preventSubmit
    , class PreventDefaultEnter, preventEnter
    , class PreventDefaultClick, preventClick
    , class LogToConsole, logShow
    , class DateTime, parseDate, now
    , class FocusElement, focusElement
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
component :: forall i o m
    . LogToConsole m
   => PreventDefaultSubmit m
   => PreventDefaultEnter m
   => PreventDefaultClick m
   => DateTime m
   => Server m
   => FocusElement m
   => H.Component HH.HTML Query i o m
component =
    H.lifecycleComponent
        { initialState: const initial
        , render
        , eval
        , receiver: const Nothing
        , initializer: Just $ Initialize unit
        , finalizer: Nothing
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
    { account :: Maybe String
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


-- TODO: Break out into TripStopQuery & TripTransactionQuery types.
data Query a
    = Initialize a
    -- ^ Get the current date & set the Date & Trip Number fields.
    | InputCompany String a
    | InputDate String a
    | InputName String a
    | InputNumber String a
    | InputAdvance String a
    | InputReturned String a
    | RemoveStop Int a
    | AddStop a
    | StopInputName Int String a
    | StopInputTotal Int String a
    | StopAddRows Int ME.MouseEvent a
    | TransactionInputMemo Int Int String a
    | TransactionInputAmount Int Int String a
    | TransactionInputTax Int Int String a
    | TransactionInputTotal Int Int String a
    | TransactionCheckReturn Int Int Boolean a
    | TransactionClickRemove Int Int ME.MouseEvent a
    | TransactionInputEnter Int Int KE.KeyboardEvent a
    | SubmitForm E.Event a
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
   => Query ~> H.ComponentDSL State Query o m
eval = case _ of
    Initialize next -> do
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
        pure next
    InputCompany str next -> do
        H.modify_ (_ { company = Just str })
        case Int.fromString str of
            Just companyId ->
                fetchAccounts companyId
            Nothing ->
                pure unit
        pure next
    InputDate str next -> do
        autofillTripNumber str
        H.modify_ (_ { date = Just str })
        pure next
    InputName str next -> do
        H.modify_ (_ { name = Just str })
        pure next
    InputNumber str next -> do
        H.modify_ (_ { tripNumber = Just str })
        pure next
    InputAdvance str next -> do
        H.modify_ (_ { cashAdvance = Just str })
        pure next
    InputReturned str next -> do
        H.modify_ (_ { cashReturned = Just str })
        pure next
    RemoveStop index next -> do
        deleteStop index
        pure next
    AddStop next -> do
        H.modify_ $ \st -> st { stops = st.stops <> [ initialStop ] }
        pure next
    StopInputName index str next -> do
        updateStop index (_ { name = Just str })
        pure next
    StopInputTotal index str next -> do
        updateStop index (_ { stopTotal = Just str })
        pure next
    StopAddRows index ev next -> do
        addRowsToStop index 3
        preventClick ev
        pure next
    TransactionInputMemo stopIndex transIndex str next -> do
        updateTransaction stopIndex transIndex (_ { memo = Just str })
        pure next
    TransactionInputAmount stopIndex transIndex str next -> do
        updateTransaction stopIndex transIndex (_ { amount = Just str })
        recalculateTransactionTotal stopIndex transIndex
        pure next
    TransactionInputTax stopIndex transIndex str next -> do
        updateTransaction stopIndex transIndex (_ { tax = Just str })
        recalculateTransactionTotal stopIndex transIndex
        pure next
    TransactionInputTotal stopIndex transIndex str next -> do
        updateTransaction stopIndex transIndex (_ { total = Just str })
        clearAmountAndTax stopIndex transIndex
        pure next
    TransactionCheckReturn stopIndex transIndex val next -> do
        updateTransaction stopIndex transIndex (_ { isReturn = val })
        pure next
    TransactionClickRemove stopIndex transIndex ev next -> do
        deleteTransaction stopIndex transIndex
        preventClick ev
        pure next
    TransactionInputEnter stopIndex transIndex ev next -> do
        -- TODO: focus account select once implemented
        wasPrevented <- preventEnter ev
        when wasPrevented $ do
            st <- H.get
            for_ (Array.index st.stops stopIndex) \stop -> do
                let transactionCount = Array.length stop.transactions
                when (transactionCount - 1 <= transIndex)
                    $ addRowsToStop stopIndex 1
                focusStopTransaction stopIndex (transIndex + 1)
        pure next
    SubmitForm ev next -> do
        st <- H.get
        logShow st
        preventSubmit ev
        pure next
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
    -- Change focus to the first input of a TripStop's Transaction.
    focusStopTransaction :: forall s f i m_
        . FocusElement m_
       => Int -> Int -> H.ComponentDSL s f i m_ Unit
    focusStopTransaction stopIndex transIndex = do
        H.getHTMLElementRef (transactionDetailRef stopIndex transIndex)
            >>= traverse_ focusElement


-- | Render the Add a Trip form.
render :: State -> H.ComponentHTML Query
render st =
    HH.form [ HE.onSubmit $ HE.input SubmitForm ] $
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
        <> Array.mapWithIndex (renderTripStop st.errors) st.stops
        <> [ button "Add Stop" HP.ButtonButton (H.ClassName "primary") AddStop
           , submitButton "Submit Trip"
           ]
  where
    errors field = V.getFieldErrors field st.errors


-- | Render the fieldset for a TripStop.
renderTripStop :: V.FormErrors -> Int -> TripStop -> H.ComponentHTML Query
renderTripStop formErrors stopIndex tripStop =
    HH.fieldset_
        [ HH.legend_ [ HH.text stopLabel ]
        , input "Stop Name" HP.InputText tripStop.name (StopInputName stopIndex) (errors "name")
            "The name of the store."
        , dollarInput "Total Spent" tripStop.stopTotal (StopInputTotal stopIndex) (errors "total")
            "The amount spent at the store."
        , renderTransactionTable formErrors stopIndex tripStop.stopTotal tripStop.transactions
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

renderTransactionTable :: V.FormErrors -> Int -> Maybe String -> Array Transaction -> H.ComponentHTML Query
renderTransactionTable formErrors stopIndex stopTotal transactions =
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
            $ Array.mapWithIndex (renderTransaction formErrors stopIndex)
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
    addRowsCell :: forall p. H.HTML p Query
    addRowsCell =
        HH.td [ HP.colSpan 7 ]
            [ HH.small_
                [ HH.a
                    [ HP.href "#"
                    , HP.title "Add Rows"
                    , HE.onClick $ HE.input $ StopAddRows stopIndex
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
renderTransaction :: V.FormErrors -> Int -> Int -> Transaction -> H.ComponentHTML Query
renderTransaction formErrors stopIndex transactionIndex transaction =
    HH.tr_ $ map centeredCell
        [ [ HH.text "ACCOUNT SELECT!" ]
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
                [ HE.onClick $ HE.input (mkAction TransactionClickRemove)
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
    centeredCell :: forall p i. Array (H.HTML p i) -> H.HTML p i
    centeredCell =
        HH.td [ HP.class_ $ H.ClassName "align-center" ]

transactionDetailRef :: Int -> Int -> H.RefLabel
transactionDetailRef stopI transI =
    H.RefLabel
        $ "__newtrip_transaction_detail_" <> show stopI <> "_" <> show transI


-- Inputs

-- TODO: On Enter, raise message that moves to next row or adds new row
tableInput :: forall p i
    . String
   -> Maybe String
   -> (String -> Unit -> i Unit)
   -> (KE.KeyboardEvent -> Unit -> i Unit)
   -> Boolean
   -> Array (H.IProp HTMLinput i)
   -> HH.HTML p (i Unit)
tableInput name value action enterKeyAction hasError attrs =
    HH.input $
        [ HP.type_ HP.InputText
        , HP.name name
        , HE.onValueInput $ HE.input action
        , HE.onKeyDown $ HE.input enterKeyAction
        ] <> optionalValue value <> errorClass hasError <> attrs

-- TODO: On Enter, raise message that moves to next row or adds new row
tableAmountInput :: forall p i
    . String
   -> Maybe String
   -> (String -> Unit -> i Unit)
   -> (KE.KeyboardEvent -> Unit -> i Unit)
   -> Boolean
   -> HH.HTML p (i Unit)
tableAmountInput name value action enterKeyAction hasError =
    HH.input $
        [ HP.type_ HP.InputNumber
        , HP.name name
        , HE.onValueInput $ HE.input action
        , HE.onKeyDown $ HE.input enterKeyAction
        , HP.min 0.0
        , HP.step $ HP.Step 0.01
        ] <> optionalValue value <> errorClass hasError

-- TODO: On Enter, raise message that moves to next row or adds new row
tableCheckbox :: forall p i
    . String
   -> Boolean
   -> (Boolean -> Unit -> i Unit)
   -> (KE.KeyboardEvent -> Unit -> i Unit)
   -> HH.HTML p (i Unit)
tableCheckbox name value action enterKeyAction =
    HH.input $
        [ HP.type_ HP.InputCheckbox
        , HP.name name
        , HE.onChecked $ HE.input action
        , HE.onKeyDown $ HE.input enterKeyAction
        , HP.checked value
        ]

errorClass :: forall r i. Boolean -> Array (H.IProp ( class :: String | r ) i)
errorClass hasError =
    if hasError then
        [ HP.class_ $ H.ClassName "error" ]
    else
        []

-- | Render a disabled input for the Cash Spent, calculated from the Trip
-- | Advance and the Cash Returned.
cashSpentInput :: forall i. State -> H.ComponentHTML i
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
companySelect :: Maybe String -> Array CompanyData -> H.ComponentHTML Query
companySelect selectedCompany companies =
    labelWrapper "Company" [] "The company to make a trip for."
        $ HH.select
            [ HP.required true
            , HP.autofocus true
            , HE.onValueChange $ HE.input InputCompany
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
