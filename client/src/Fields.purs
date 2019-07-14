module Fields
    ( UTCDate(..)
    , Cents
    , decimalToCents
    , Percentage
    , decimalToPercentage
    ) where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode ((:=), (~>), class EncodeJson, encodeJson)
import Data.Date (Date, year, month, day)
import Data.Decimal as Decimal
import Data.Enum (fromEnum)
import Data.Foldable (intercalate)
import Data.Int as Int


-- | Date for conversion to an API UTCTime.
data UTCDate
    = UTCDate Date

instance encodeUTCDate :: EncodeJson UTCDate where
    encodeJson (UTCDate date) =
        encodeJson $ intercalate "-"
            [ show $ fromEnum $ year date
            , showAndPad $ fromEnum $ month date
            , showAndPad $ fromEnum $ day date
            ]
            <> "T00:00:00Z"

-- Pad the string from a month/date so it is 2 characters long.
showAndPad :: Int -> String
showAndPad num = if num < 10 then "0" <> show num else show num


-- | Dollars stored as an integer of Cents.
data Cents
    = Cents Int

instance encodeCents :: EncodeJson Cents where
    encodeJson (Cents cents) =
           "cents" := cents
        ~> jsonEmptyObject

-- | Round a decimal to 2 places and wrap it in Cents.
decimalToCents :: Decimal.Decimal -> Cents
decimalToCents =
    Cents <<< hundredthsOfDecimal


-- | Percentages stored as an integer at the 2nd decimal place.
data Percentage
    = Percentage Int

instance encodePercentage :: EncodeJson Percentage where
    encodeJson (Percentage percent) =
           "percentage" := percent
        ~> jsonEmptyObject

decimalToPercentage :: Decimal.Decimal -> Percentage
decimalToPercentage =
    Percentage <<< hundredthsOfDecimal


-- | Transform a decimal into the integer value of it's hundreths amount.
-- |
-- | > hundredthsOfDecimal 17.02 == 1702
hundredthsOfDecimal :: Decimal.Decimal -> Int
hundredthsOfDecimal dec =
    Int.round $ Decimal.toNumber $ Decimal.round $ dec * Decimal.fromInt 100
