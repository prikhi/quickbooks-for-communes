{- | The page component for adding a new Trip Entry.

TODO:
    * Company dropdown + account fetching.
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
import Data.Date (Date, year, month, day)
import Data.DateTime (date)
import Data.Decimal as Decimal
import Data.Enum (fromEnum)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as E

import App
    ( class PreventDefaultSubmit, preventSubmit
    , class LogToConsole, logShow
    , class DateTime, parseDate, now
    )
import Forms (input, dateInput, dollarInput, submitButton, labelWrapper, optionalValue)
import Validation as V


-- | The "Add a Trip" Page.
component :: forall i o m
    . LogToConsole m
   => PreventDefaultSubmit m
   => DateTime m
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
    { date :: Maybe String
    , name :: Maybe String
    , tripNumber :: Maybe String
    , cashAdvance :: Maybe String
    , cashReturned :: Maybe String
    , errors :: V.FormErrors
    }

initial :: State
initial =
    { date: Nothing
    , name: Nothing
    , tripNumber: Nothing
    , cashAdvance: Nothing
    , cashReturned: Nothing
    , errors: V.empty
    }


data Query a
    = Initialize a
    -- ^ Get the current date & set the Date & Trip Number fields.
    | InputDate String a
    | InputName String a
    | InputNumber String a
    | InputAdvance String a
    | InputReturned String a
    | SubmitForm E.Event a
    -- ^ Validate & POST the form.

-- | Initialize, update, & submit the form.
eval :: forall m
    . MonadState State m
   => LogToConsole m
   => PreventDefaultSubmit m
   => DateTime m
   => Query ~> m
eval = case _ of
    Initialize next -> do
        currentDate <- date <$> now
        H.modify_ (_ { date = Just $ makeDate currentDate })
        updateTripNumber currentDate
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


-- | Render the Add a Trip form.
render :: State -> H.ComponentHTML Query
render st =
    HH.form [ HE.onSubmit $ HE.input SubmitForm ]
        [ dateInput "Date" st.date InputDate (errors "date")
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
        , submitButton "Submit Trip"
        ]
  where
    errors field = V.getFieldErrors field st.errors

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
  where
    toDecimal :: Maybe String -> Maybe Decimal.Decimal
    toDecimal = join <<< map Decimal.fromString
