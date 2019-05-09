{- | The page component for adding a new Trip Entry.

TODO:
    * Handle waiting for companies to load & no companies returned
    * Handle unselected company, waiting for accounts to load & no accounts
      returned
    * Form validation
    * Form submission
    * Turn <a href="#"> "Add Rows" links into buttons styled like links
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
import Data.Foldable (fold, traverse_, notElem)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
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
import Web.UIEvent.KeyboardEvent.EventTypes as KET

import AccountSelect as AccountSelect
import App
    ( class PreventDefaultSubmit, preventSubmit
    , class PreventDefaultEnter, preventEnter
    , class PreventDefaultClick, preventClick
    , class LogToConsole, logShow
    , class DateTime, parseDate, now
    , class FocusElement, focusElement
    , class SelectComponent
    )
import Forms
    ( input, dateInput, dollarInput, submitButton, labelWrapper, optionalValue
    , button
    )
import Server
    ( CompanyData(..), AccountData, TripStoreAccount(..)
    , class Server, companiesRequest, accountsRequest, tripStoreRequest
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
    , storeAccounts :: Array TripStoreAccount
    , company :: Maybe String
    , date :: Maybe String
    , name :: Maybe String
    , tripNumber :: Maybe String
    , cashAdvance :: Maybe String
    , cashReturned :: Maybe String
    , stops :: Array TripStop
    , storeCreditStops :: Array StoreCreditStop
    , stopCounter :: StopCount
    , errors :: V.FormErrors
    }

initial :: State
initial =
    { companies: []
    , accounts: []
    , storeAccounts: []
    , company: Nothing
    , date: Nothing
    , name: Nothing
    , tripNumber: Nothing
    , cashAdvance: Nothing
    , cashReturned: Nothing
    , stops: [ initialStop $ StopCount 0 ]
    , storeCreditStops: []
    , stopCounter: StopCount 1
    , errors: V.empty
    }

type TripStop =
    { name :: Maybe String
    , stopTotal :: Maybe String
    , transactions :: Array Transaction
    , stopCount :: StopCount
    , transactionCounter :: TransactionCount
    }

initialStop :: StopCount -> TripStop
initialStop stopCount =
    { name: Nothing
    , stopTotal: Nothing
    , transactions: map (TransactionCount >>> initialTransaction) $ Array.range 0 2
    , stopCount
    , transactionCounter: TransactionCount 3
    }

type Transaction =
    { account :: Maybe AccountData
    , memo :: Maybe String
    , amount :: Maybe String
    , tax :: Maybe String
    , total :: Maybe String
    , isReturn :: Boolean
    , transactionCount :: TransactionCount
    }

initialTransaction :: TransactionCount -> Transaction
initialTransaction transactionCount =
    { account: Nothing
    , memo: Nothing
    , amount: Nothing
    , tax: Just "5.3"
    , total: Nothing
    , isReturn: false
    , transactionCount
    }

type StoreCreditStop =
    { storeAccount :: Maybe TripStoreAccount
    , stopCount :: StopCount
    , stopTotal :: Maybe String
    , transactions :: Array StoreCreditTransaction
    , transactionCounter :: TransactionCount
    }

initialStoreCreditStop :: Array TripStoreAccount -> StopCount -> StoreCreditStop
initialStoreCreditStop storeAccounts stopCount =
    { storeAccount: Array.head storeAccounts
    , stopCount
    , stopTotal: Nothing
    , transactions: [initialStoreCreditTransaction $ TransactionCount 0]
    , transactionCounter: TransactionCount 1
    }

type StoreCreditTransaction =
    { account :: Maybe AccountData
    , memo :: Maybe String
    , amount :: Maybe String
    , tax :: Maybe String
    , total :: Maybe String
    , transactionCount :: TransactionCount
    }

initialStoreCreditTransaction :: TransactionCount -> StoreCreditTransaction
initialStoreCreditTransaction transactionCount =
    { account: Nothing
    , memo: Nothing
    , amount: Nothing
    , tax: Just "5.3"
    , total: Nothing
    , transactionCount
    }

-- | An increasing counter for the form's TripStops. Provides a unique index
-- | for the AccountSelect component without relying on the Array position.
data StopCount
    = StopCount Int
derive instance stopCountGeneric :: Generic StopCount _
derive instance stopCountEq :: Eq StopCount
derive instance stopCountOrd :: Ord StopCount
instance stopCountShow :: Show StopCount where
    show = genericShow

-- | Get the next StopCount & increment the State's stopCounter.
nextStopCount :: forall m. MonadState State m => m StopCount
nextStopCount = do
    nextCounter <- H.gets (_.stopCounter)
    H.modify_ \st -> st
        { stopCounter = (\(StopCount c) -> StopCount $ c + 1) st.stopCounter
        }
    pure nextCounter

-- | An increasing counter for a TripStop's Transactions. Provides a unique
-- | index for the AccountSelect component without relying on the Array
-- | position.
data TransactionCount
    = TransactionCount Int
derive instance transCountGeneric :: Generic TransactionCount _
derive instance transCountEq :: Eq TransactionCount
derive instance transCountOrd :: Ord TransactionCount
instance transCountShow :: Show TransactionCount where
    show = genericShow

-- | Append a new Transaction & increase the stop's transactionCounter.
addTransaction :: TripStop -> TripStop
addTransaction =
    genericAddTransaction initialTransaction

-- | Append a new StoreCreditTransaction & increase the stop's
-- | transactionCounter.
addCreditTransaction :: StoreCreditStop -> StoreCreditStop
addCreditTransaction =
    genericAddTransaction initialStoreCreditTransaction

-- | Generically build & append a Transaction and increase the stop's
-- | transactionCounter.
genericAddTransaction :: forall trans stop
    .  (TransactionCount -> trans)
   -> { transactionCounter :: TransactionCount, transactions :: Array trans
      | stop
      }
   -> { transactionCounter :: TransactionCount, transactions :: Array trans
      | stop
      }
genericAddTransaction transactionMaker stop = stop
    { transactions =
        stop.transactions <> [ transactionMaker stop.transactionCounter ]
    , transactionCounter =
        (\(TransactionCount c) -> TransactionCount $ c + 1) stop.transactionCounter
    }

-- | Sum the total amount for every Transaction in the TripStop.
stopTransactionTotal :: TripStop -> Decimal.Decimal
stopTransactionTotal =
    genericStopTransactionTotal \trans ->
        Decimal.fromInt $
            if trans.isReturn
                then -1
                else 1

creditStopTransactionTotal :: StoreCreditStop -> Decimal.Decimal
creditStopTransactionTotal =
    genericStopTransactionTotal (const $ Decimal.fromInt 1)

genericStopTransactionTotal :: forall stop trans
    . ({ total :: Maybe String | trans } -> Decimal.Decimal)
   -> { transactions :: Array { total :: Maybe String | trans } | stop }
   -> Decimal.Decimal
genericStopTransactionTotal getMultiplier { transactions } =
    Array.foldl sumTotals (Decimal.fromInt 0) transactions
  where
    sumTotals :: Decimal.Decimal -> { total :: Maybe String | trans } -> Decimal.Decimal
    sumTotals acc transaction =
        fromMaybe acc $
            (+) <$> pure acc
                <*> map (\d -> getMultiplier transaction * d) (toDecimal transaction.total)

-- | Calculate the Cash Spent from the Trip's Advance & Return amounts.
entryCashSpent :: State -> Maybe Decimal.Decimal
entryCashSpent st =
    (-) <$> toDecimal st.cashAdvance
        <*> toDecimal st.cashReturned


-- | Slots for each Transaction's AccountSelect component. These use the
-- | StopCount & TransactionCount types instead of indexes to provide unique
-- | slots to each Transaction, even when Transaction rows have been removed.
type ChildSlots =
    ( accountSelect :: AccountSelect.Slot (Tuple StopCount TransactionCount)
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
    | AddCreditStop
    | SubmitForm E.Event
    -- ^ Validate & POST the form.
    -- Trip Stop Actions
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
    -- Store Credit Stop Actions
    | RemoveCreditStop Int
    | CreditInputStore Int String
    | CreditInputTotal Int String
    | CreditStopAddRows Int ME.MouseEvent
    | CreditTransactionHandleAccount Int Int AccountSelect.Message
    | CreditTransactionInputMemo Int Int String
    | CreditTransactionInputAmount Int Int String
    | CreditTransactionInputTax Int Int String
    | CreditTransactionInputTotal Int Int String
    | CreditTransactionClickRemove Int Int ME.MouseEvent
    | CreditTransactionInputEnter Int Int KE.KeyboardEvent

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
    InputName str ->
        H.modify_ (_ { name = Just str })
    InputNumber str ->
        H.modify_ (_ { tripNumber = Just str })
    InputAdvance str ->
        H.modify_ (_ { cashAdvance = Just str })
    InputReturned str ->
        H.modify_ (_ { cashReturned = Just str })
    RemoveStop index ->
        deleteStop index
    AddStop -> do
        stopCount <- nextStopCount
        H.modify_ $ \st -> st
            { stops = st.stops <> [ initialStop stopCount ]
            }
        focusLastStop (_.stops) stopNameFieldRef
    AddCreditStop -> do
        stopCount <- nextStopCount
        H.modify_ $ \st -> st
            { storeCreditStops =
                st.storeCreditStops
                    <> [ initialStoreCreditStop st.storeAccounts stopCount ]
            }
        focusLastStop (_.storeCreditStops) creditStopAccountFieldRef
    SubmitForm ev -> do
        st <- H.get
        logShow st
        preventSubmit ev
    StopInputName index str ->
        updateStop index (_ { name = Just str })
    StopInputTotal index str ->
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
            preventEnter event *> focusNextTransaction stopIndex transIndex
    TransactionInputMemo stopIndex transIndex str ->
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
    TransactionCheckReturn stopIndex transIndex val ->
        updateTransaction stopIndex transIndex (_ { isReturn = val })
    TransactionClickRemove stopIndex transIndex ev -> do
        deleteTransaction stopIndex transIndex
        preventClick ev
    TransactionInputEnter stopIndex transIndex ev ->
        preventEnter ev *> focusNextTransaction stopIndex transIndex
    RemoveCreditStop index ->
        deleteCreditStop index
    CreditInputStore index str -> do
        st <- H.get
        case Int.fromString str of
            Just storeId ->
                updateCreditStop index \stop ->
                    case Array.find (\(TripStoreAccount a) -> a.id == storeId) st.storeAccounts of
                        Just tsa ->
                            stop { storeAccount = Just tsa }
                        Nothing ->
                            stop
            Nothing ->
                pure unit
    CreditInputTotal index str ->
        updateCreditStop index (_ { stopTotal = Just str })
    CreditStopAddRows index ev -> do
        addRowsToCreditStop index 3
        preventClick ev
    CreditTransactionHandleAccount stopIndex transIndex msg -> case msg of
        AccountSelect.Selected acc ->
            updateCreditTransaction stopIndex transIndex (_ { account = Just acc })
        AccountSelect.Cleared ->
            updateCreditTransaction stopIndex transIndex (_ { account = Nothing })
        AccountSelect.EnterKeyDownWhileClosed event ->
            preventEnter event *> focusNextCreditTransaction stopIndex transIndex
    CreditTransactionInputMemo stopIndex transIndex str ->
        updateCreditTransaction stopIndex transIndex (_ { memo = Just str })
    CreditTransactionInputAmount stopIndex transIndex str -> do
        updateCreditTransaction stopIndex transIndex (_ { amount = Just str })
        recalculateCreditTransactionTotal stopIndex transIndex
    CreditTransactionInputTax stopIndex transIndex str -> do
        updateCreditTransaction stopIndex transIndex (_ { tax = Just str })
        recalculateCreditTransactionTotal stopIndex transIndex
    CreditTransactionInputTotal stopIndex transIndex str -> do
        updateCreditTransaction stopIndex transIndex (_ { total = Just str })
        clearCreditAmountAndTax stopIndex transIndex
    CreditTransactionClickRemove stopIndex transIndex ev -> do
        deleteCreditTransaction stopIndex transIndex
        preventClick ev
    CreditTransactionInputEnter stopIndex transIndex ev ->
        preventEnter ev *> focusNextCreditTransaction stopIndex transIndex

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
        tripStoreRequest companyId >>= case _ of
            Right storeAccs ->
                H.modify_ (_ { storeAccounts = storeAccs })
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
        replicateM count $ addRowToStop index
    -- Repeat a monadic action.
    replicateM :: forall m_ a. Monad m_ => Int -> m_ a -> m_ Unit
    replicateM count action =
        if count > 0
            then action *> replicateM (count - 1) action
            else pure unit
    -- Add a single transaction to a TripStop, incresing it's transactionCounter.
    addRowToStop :: forall m_. MonadState State m_ => Int -> m_ Unit
    addRowToStop index =
        updateStop index addTransaction
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
    recalculateTransactionTotal :: forall m_
        . MonadState State m_ => Int -> Int -> m_ Unit
    recalculateTransactionTotal stopIndex transIndex =
        recalculateTotal $ updateTransaction stopIndex transIndex
    recalculateCreditTransactionTotal :: forall m_
        . MonadState State m_ => Int -> Int -> m_ Unit
    recalculateCreditTransactionTotal stopIndex transIndex =
        recalculateTotal $ updateCreditTransaction stopIndex transIndex
    -- Set the Total Amount for a Transaction if it's Amount is entered.
    recalculateTotal :: forall m_ trans
        . ( ( { amount :: Maybe String, tax :: Maybe String, total :: Maybe String | trans }
                -> { amount :: Maybe String, tax :: Maybe String, total :: Maybe String | trans }
            )
         -> m_ Unit
          )
       -> m_ Unit
    recalculateTotal updater =
        updater $ \transaction ->
            transaction { total = map (Decimal.toFixed 2) $ calculateTotal transaction }
    -- Calculate the total from the Tax & Amount, Defaulting the Tax to 0% if
    -- not present.
    calculateTotal :: forall trans
        . { amount :: Maybe String, tax :: Maybe String | trans }
       -> Maybe Decimal.Decimal
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
    -- Focus the Transaction after the one with the given index.
    focusNextTransaction :: forall f o_ m_
        . Int -> Int -> H.HalogenM State f ChildSlots o_ m_ Unit
    focusNextTransaction =
        focusNextRow (_.stops) addRowToStop
    -- Focus the StoreCreditTransaction after the one with the given index.
    focusNextCreditTransaction :: forall f o_ m_
        . Int -> Int -> H.HalogenM State f ChildSlots o_ m_ Unit
    focusNextCreditTransaction =
        focusNextRow (_.storeCreditStops) addRowToCreditStop
    -- A generalized function for focusing the transaction row after the given
    -- one - adding a new row if the specific transaction index is the last
    -- row. This works for both TripStops & StoreCreditStops by passing a stops
    -- selector for the State and a function to add a new row to the stop type.
    focusNextRow :: forall f o_ m_ trans stop
        . ( State
            -> Array
                { stopCount :: StopCount
                , transactions ::
                    Array { transactionCount :: TransactionCount | trans }
                | stop
                }
          )
       -> (Int -> H.HalogenM State f ChildSlots o_ m_ Unit)
       -> Int
       -> Int
       -> H.HalogenM State f ChildSlots o_ m_ Unit
    focusNextRow modelSelector rowAdder stopIndex transactionIndex = do
        stops <- H.gets modelSelector
        for_ (Array.index stops stopIndex) \stop -> do
            let transactionCount = Array.length stop.transactions
            when (transactionCount - 1 <= transactionIndex)
                $ rowAdder stopIndex
            focusStopTransaction modelSelector stopIndex (transactionIndex + 1)
    -- A generalized function for changing focus to the first input of a
    -- TripStop or StoreCreditStop's Transaction.
    focusStopTransaction :: forall f o_ m_ trans stop
        . ( State
            -> Array
                { stopCount :: StopCount
                , transactions ::
                    Array { transactionCount :: TransactionCount | trans }
                | stop
                }
          )
        -> Int
        -> Int
        -> H.HalogenM State f ChildSlots o_ m_ Unit
    focusStopTransaction modelSelector stopIndex transIndex = do
        stops <- H.gets modelSelector
        case Array.index stops stopIndex of
            Just stop ->
                case Array.index stop.transactions transIndex of
                    Just transaction -> void $
                        H.query _accountSelect
                            (Tuple stop.stopCount transaction.transactionCount)
                            $ AccountSelect.Focus unit
                    Nothing ->
                        pure unit
            Nothing ->
                pure unit
    -- Focus a field of the last stop, given the stop selector & function to
    -- generate an element reference.
    focusLastStop :: forall m_ a f p o_
        . FocusElement m_
       => (State -> Array a)
       -> (Int -> H.RefLabel)
       -> H.HalogenM State f p o_ m_ Unit
    focusLastStop modelSelector fieldRefFunction =
        (modelSelector >>> Array.length >>> (\x -> x - 1)) <$> H.get
            >>= fieldRefFunction >>> H.getHTMLElementRef
            >>= traverse_ focusElement
    -- Update a StoreCreditStop, given it's index.
    updateCreditStop :: forall m_
        . MonadState State m_
       => Int -> (StoreCreditStop -> StoreCreditStop) -> m_ Unit
    updateCreditStop index updater =
        H.modify_ \st -> st
            { storeCreditStops =
                fromMaybe st.storeCreditStops
                    $ Array.modifyAt index updater st.storeCreditStops
            }
    -- Add 3 Transaction rows to the StoreCreditStop at the given index.
    addRowsToCreditStop :: forall m_. MonadState State m_ => Int -> Int -> m_ Unit
    addRowsToCreditStop index count =
        replicateM count $ addRowToCreditStop index
    -- Add a Transaction row to the StoreCreditStop at the given index.
    addRowToCreditStop :: forall m_. MonadState State m_ => Int -> m_ Unit
    addRowToCreditStop index =
        updateCreditStop index addCreditTransaction
    -- Update the StoreCreditTransaction at the given stop & transaction index.
    updateCreditTransaction :: forall m_
        . MonadState State m_
       => Int
       -> Int
       -> (StoreCreditTransaction -> StoreCreditTransaction)
       -> m_ Unit
    updateCreditTransaction stopIndex transIndex updater =
        updateCreditStop stopIndex \stop -> stop
            { transactions = fromMaybe stop.transactions
                $ Array.modifyAt transIndex updater stop.transactions
            }
    clearCreditAmountAndTax :: forall m_
        . MonadState State m_ => Int -> Int -> m_ Unit
    clearCreditAmountAndTax stopIndex transIndex =
        updateCreditTransaction stopIndex transIndex
            (_ { amount = Nothing, tax = Nothing })
    deleteCreditTransaction :: forall m_
        . MonadState State m_ => Int -> Int -> m_ Unit
    deleteCreditTransaction stopIndex transIndex =
        updateCreditStop stopIndex \stop -> stop
            { transactions = fromMaybe stop.transactions
                $ Array.deleteAt transIndex stop.transactions
            }
    deleteCreditStop :: forall m_. MonadState State m_ => Int -> m_ Unit
    deleteCreditStop index =
        H.modify_ \st -> st
            { storeCreditStops =
                fromMaybe st.storeCreditStops
                    $ Array.deleteAt index st.storeCreditStops
            }


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
           ]
        <> Array.mapWithIndex (renderStoreCreditStop st.accounts st.storeAccounts st.errors)
            st.storeCreditStops
        <> [ button "Add Store Credit Stop" HP.ButtonButton (H.ClassName "primary") AddCreditStop
           , submitButton "Submit Trip"
           , renderEntryOutOfBalance st
           ]
  where
    errors :: String -> Array String
    errors field = V.getFieldErrors field st.errors


renderEntryOutOfBalance :: forall f g m. State -> H.ComponentHTML f g m
renderEntryOutOfBalance st =
    HH.dl [ HP.classes
                $ H.ClassName "entry-out-of-balance-container"
                    `Array.cons` outOfBalanceClass balanceable outOfBalance
          ]
        [ HH.dt_ [ HH.text "Out of Balance:" ]
        , HH.dd_ [ HH.text $ "$" <> Decimal.toFixed 2 outOfBalance ]
        ]
  where
    balanceable :: Boolean
    balanceable =
        entryCashSpent st /= Nothing
            || transactionTotal /= Decimal.fromInt 0
    transactionTotal :: Decimal.Decimal
    transactionTotal =
        Array.foldl (\acc stop -> acc + stopTransactionTotal stop) (Decimal.fromInt 0)
            st.stops
    outOfBalance :: Decimal.Decimal
    outOfBalance =
            fromMaybe (Decimal.fromInt 0) (entryCashSpent st)
                - transactionTotal


-- Trip Stops

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
        , nameInput
        , dollarInput "Total Spent" tripStop.stopTotal (StopInputTotal stopIndex) (errors "total")
            "The amount spent at the store."
        , renderTransactionTable
            accounts
            formErrors
            stopIndex
            tripStop
        , button "Remove Stop" HP.ButtonButton (H.ClassName "danger") (RemoveStop stopIndex)
        ]
  where
    errors :: String -> Array String
    errors field =
        V.getFieldErrors ("trip-stop-" <> show stopIndex <> "-" <> field)
            formErrors
    stopLabel :: String
    stopLabel = case tripStop.name of
        Nothing ->
            "Trip Stop"
        Just "" ->
            "Trip Stop"
        Just str ->
            "Trip Stop: " <> str
    nameInput :: forall m_ g. H.ComponentHTML Action g m_
    nameInput =
        labelWrapper "Stop Name" (errors "name") "The name of the store." $
            HH.input $
                [ HP.type_ HP.InputText
                , HP.required true
                , HE.onValueInput $ Just <<< StopInputName stopIndex
                , HP.ref $ stopNameFieldRef stopIndex
                ] <> optionalValue tripStop.name

stopNameFieldRef :: Int -> H.RefLabel
stopNameFieldRef stopIndex =
    H.RefLabel
        $ "__newtrip_tripstop_name_" <> show stopIndex

renderTransactionTable :: forall m
    . SelectComponent m
   => PreventDefaultEnter m
   => Array AccountData
   -> V.FormErrors
   -> Int
   -> TripStop
   -> H.ComponentHTML Action ChildSlots m
renderTransactionTable accounts formErrors stopIndex stop@{ stopCount, stopTotal, transactions } =
  let
    transactionTotal =
        stopTransactionTotal stop
    outOfBalance =
        fromMaybe (Decimal.fromInt 0) (toDecimal stopTotal) - transactionTotal
    balanceable =
        toDecimal stopTotal `notElem` [ Nothing, Just $ Decimal.fromInt 0 ]
            || transactionTotal /= Decimal.fromInt 0
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
            $ Array.mapWithIndex (renderTransaction accounts formErrors stopCount stopIndex)
                transactions
        , HH.tfoot_
            [ HH.tr_ [ addRowsCell ]
            , HH.tr_
                [ HH.th [ HP.colSpan 4 ] [ HH.text "Total:"]
                , HH.td [ HP.colSpan 3 ] [ HH.text $ "$" <> Decimal.toFixed 2 transactionTotal]
                ]
            , HH.tr [ HP.classes $ outOfBalanceClass balanceable outOfBalance ]
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


-- | Render the form row for a Transaction.
-- | TODO: Highlight errors & render another row below w/ messages.
renderTransaction :: forall m
    . SelectComponent m
   => PreventDefaultEnter m
   => Array AccountData
   -> V.FormErrors
   -> StopCount
   -> Int
   -> Int
   -> Transaction
   -> H.ComponentHTML Action ChildSlots m
renderTransaction accounts formErrors stopCount stopIndex transactionIndex transaction =
    HH.tr_ $ map centeredCell
        [ [ HH.slot _accountSelect (Tuple stopCount transaction.transactionCount)
            AccountSelect.component accounts
            $ Just <<< TransactionHandleAccount stopIndex transactionIndex
          ]
        , [ tableInput "memo" transaction.memo
            (mkAction TransactionInputMemo)
            (mkAction TransactionInputEnter)
            false
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

-- | Build a table cell with centered content.
centeredCell :: forall p i. Array (HH.HTML p i) -> HH.HTML p i
centeredCell =
    HH.td [ HP.class_ $ H.ClassName "align-center" ]


-- Store Credit Stops

-- | Render the fieldset for a StoreCreditStop.
renderStoreCreditStop :: forall m
    . SelectComponent m
   => PreventDefaultEnter m
   => Array AccountData
   -> Array TripStoreAccount
   -> V.FormErrors
   -> Int
   -> StoreCreditStop
   -> H.ComponentHTML Action ChildSlots m
renderStoreCreditStop accounts storeAccounts formErrors stopIndex creditStop =
    HH.fieldset_
        [ HH.legend_ [ HH.text stopLabel ]
        , storeSelect
        , dollarInput "Total Spent" creditStop.stopTotal (CreditInputTotal stopIndex)
            (errors "total") "The amount spent at the store."
        , renderCreditTransactionTable accounts formErrors stopIndex creditStop
        , button "Remove Stop" HP.ButtonButton (H.ClassName "danger")
            (RemoveCreditStop stopIndex)
        ]
  where
    errors :: String -> Array String
    errors field =
        V.getFieldErrors ("store-credit-stop-" <> show stopIndex <> "-" <> field)
            formErrors
    stopLabel :: String
    stopLabel = case creditStop.storeAccount of
        Nothing ->
            "Store Credit Stop"
        Just (TripStoreAccount storeAcc) ->
            "Store Credit Stop: " <> storeAcc.name
    storeSelect :: forall p. HH.HTML p Action
    storeSelect =
        labelWrapper "Store" (errors "store-account") "The store purchases were made at."
            $ HH.select
                [ HP.required true
                , HP.autofocus true
                , HE.onValueChange $ Just <<< CreditInputStore stopIndex
                , HP.ref $ creditStopAccountFieldRef stopIndex
                ]
            $ map
                (\tsa@(TripStoreAccount acc) ->
                    HH.option
                        [ HP.value $ show acc.id
                        , HP.selected $ creditStop.storeAccount == Just tsa
                        ]
                        [ HH.text acc.name ]
                )
                storeAccounts

creditStopAccountFieldRef :: Int -> H.RefLabel
creditStopAccountFieldRef stopIndex =
    H.RefLabel
        $ "__newtrip_creditstop_account_" <> show stopIndex

renderCreditTransactionTable :: forall m
    . SelectComponent m
   => PreventDefaultEnter m
   => Array AccountData
   -> V.FormErrors
   -> Int
   -> StoreCreditStop
   -> H.ComponentHTML Action ChildSlots m
renderCreditTransactionTable accounts formErrors stopIndex stop@{ stopCount, stopTotal, transactions } =
  let
    transactionTotal =
          creditStopTransactionTotal stop
    outOfBalance =
        fromMaybe (Decimal.fromInt 0) (toDecimal stopTotal) - transactionTotal
    balanceable =
        toDecimal stopTotal `notElem` [ Nothing, Just $ Decimal.fromInt 0 ]
            || transactionTotal /= Decimal.fromInt 0
   in
    HH.table_
        [ HH.thead_
            [ HH.th_ [ HH.text "Account" ]
            , HH.th_ [ HH.text "Details" ]
            , HH.th_ [ HH.text "Cost" ]
            , HH.th_ [ HH.text "Tax %" ]
            , HH.th_ [ HH.text "Total Cost" ]
            , HH.th_ []
            ]
        , HH.tbody_
        $ Array.mapWithIndex (renderCreditTransaction accounts formErrors stopCount stopIndex)
            transactions
        , HH.tfoot_
            [ HH.tr_ [ addRowsCell ]
            , HH.tr_
                [ HH.th [ HP.colSpan 4 ] [ HH.text "Total:" ]
                , HH.td [ HP.colSpan 2 ] [ HH.text $ "$" <> Decimal.toFixed 2 transactionTotal ]
                ]
            , HH.tr [ HP.classes $ outOfBalanceClass balanceable outOfBalance ]
                [ HH.th [ HP.colSpan 4 ] [ HH.text "Out of Balance:" ]
                , HH.td [ HP.colSpan 2 ] [ HH.text $ "$" <> Decimal.toFixed 2 outOfBalance ]
                ]
            ]
        ]
  where
    addRowsCell :: forall p. HH.HTML p Action
    addRowsCell =
        HH.td [ HP.colSpan 6 ]
            [ HH.small_
                [ HH.a
                    [ HP.href "#"
                    , HP.title "Add Rows"
                    , HE.onClick $ Just <<< CreditStopAddRows stopIndex
                    ]
                    [ HH.text "Add Rows" ]
                ]
            ]

-- | Render the form row for a StoreCreditTransaction.
-- | TODO: Highlight errors & render another row below w/ messages.
renderCreditTransaction :: forall m
    . SelectComponent m
   => PreventDefaultEnter m
   => Array AccountData
   -> V.FormErrors
   -> StopCount
   -> Int
   -> Int
   -> StoreCreditTransaction
   -> H.ComponentHTML Action ChildSlots m
renderCreditTransaction accounts formErrors stopCount stopIndex transactionIndex transaction =
    HH.tr_ $ map centeredCell
        [ [ HH.slot _accountSelect (Tuple stopCount transaction.transactionCount)
            AccountSelect.component accounts
            $ Just <<< CreditTransactionHandleAccount stopIndex transactionIndex
          ]
        , [ tableInput "memo" transaction.memo
            (mkAction CreditTransactionInputMemo)
            (mkAction CreditTransactionInputEnter)
            false
          ]
        , [ tableInput "item-price" transaction.amount
            (mkAction CreditTransactionInputAmount)
            (mkAction CreditTransactionInputEnter)
            false
          ]
        , [ tableAmountInput "tax-rate" transaction.tax
                (mkAction CreditTransactionInputTax)
                (mkAction CreditTransactionInputEnter)
                false
          ]
        , [ tableAmountInput "item-total" transaction.total
                (mkAction CreditTransactionInputTotal)
                (mkAction CreditTransactionInputEnter)
                false
          ]
        , [ HH.a
                [ HE.onClick $ Just <<< mkAction CreditTransactionClickRemove
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


-- Inputs

tableInput :: forall p i
    . String
   -> Maybe String
   -> (String -> i)
   -> (KE.KeyboardEvent -> i)
   -> Boolean
   -> HH.HTML p i
tableInput name value action enterKeyAction hasError =
    HH.input $
        [ HP.type_ HP.InputText
        , HP.name name
        , HE.onValueInput $ Just <<< action
        , onEnterDown enterKeyAction
        ] <> optionalValue value <> errorClass hasError

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
        , onEnterDown enterKeyAction
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
    HH.label [ HP.class_ $ H.ClassName "checkbox-wrapper" ]
        [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.name name
            , HE.onChecked $ Just <<< action
            , onEnterDown enterKeyAction
            , HP.checked value
            ]
        , HH.span [ HP.class_ $ H.ClassName "checkbox-custom" ] []
        ]

-- | Trigger the action when the Enter key is pressed down.
onEnterDown :: forall i. (KE.KeyboardEvent -> i) -> HP.IProp HTMLinput i
onEnterDown action = HE.onKeyDown \keyEvent ->
    let event = KE.toEvent keyEvent in
    if E.type_ event == KET.keydown && KE.key keyEvent == "Enter" then
        Just $ action keyEvent
    else
        Nothing

errorClass :: forall r i. Boolean -> Array (HP.IProp ( class :: String | r ) i)
errorClass hasError =
    if hasError then
        [ HP.class_ $ H.ClassName "error" ]
    else
        []

outOfBalanceClass :: Boolean -> Decimal.Decimal -> Array H.ClassName
outOfBalanceClass balanceable outOfBalance =
    guard balanceable <<< Array.singleton <<< H.ClassName $
        if outOfBalance == Decimal.fromInt 0
            then "is-balanced"
            else "is-out-of-balance"

-- | Render a disabled input for the Cash Spent, calculated from the Trip
-- | Advance and the Cash Returned.
cashSpentInput :: forall a q m. State -> H.ComponentHTML a q m
cashSpentInput st =
  let cashSpent = entryCashSpent st
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
