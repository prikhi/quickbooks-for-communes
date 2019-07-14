module Validation where

import Prelude

import Affjax (Response, ResponseFormatError, printResponseFormatError)
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.State (class MonadState)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode ((.:), class DecodeJson, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Decimal as Decimal
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup as V
import Foreign.Object as Object
import Halogen as H

import App (class DateTime, parseDate)
import Fields (Cents, decimalToCents, Percentage, decimalToPercentage, UTCDate(..))


-- Form Validation

-- | Each Field can have multiple error message.
-- | TODO: newtype w/ custom monoid that merges child arrays?
type FormErrors = Map String (Array String)

-- | A record is WithErrors if it contains FormErrors in it's `errors` field.
type WithErrors s = { errors :: FormErrors | s }

-- | Validation results in some errors or the validated type.
type ValidationResult a = Either FormErrors a

-- | A validator transforms an input type into the resuls of a validation.
type Validator a b = (->) a (ValidationResult b)

-- | A validation checks and transforms some value.
type Validation a b = (->) a (V.V FormErrors b)


-- | Empty FormErrors means no errors!
empty :: FormErrors
empty = M.empty

singleton :: String -> String -> FormErrors
singleton field message =
    M.singleton field [message]

-- | Get the general errors for a form. These are found in the empty string key
-- | of the error map.
getFormErrors :: FormErrors -> Array String
getFormErrors = getFieldErrors ""

-- | Get the errors for a field.
getFieldErrors :: String -> FormErrors -> Array String
getFieldErrors field =
    M.lookup field >>> fromMaybe []

-- | Return a single error from a field validation.
singleError :: forall a. String -> String -> V.V FormErrors a
singleError field =
    singleton field >>> V.invalid

-- | Map over the FormErrors of a Validation.
mapErrors :: forall a. (FormErrors -> FormErrors) -> V.V FormErrors a -> V.V FormErrors a
mapErrors = lmap

-- | Transform all Field Names for the current Errors.
mapErrorFields :: forall a. (String -> String) -> V.V FormErrors a -> V.V FormErrors a
mapErrorFields keyModifier = mapErrors $ foldlWithIndex
    (\key newMap value -> M.insert (keyModifier key) value newMap)
    M.empty


-- | Ensure a Maybe String has a non-empty value present.
validateNonEmpty :: String -> Validation (Maybe String) String
validateNonEmpty field = case _ of
    Nothing -> singleError field "A value is required."
    Just "" -> singleError field "A value is required."
    Just s  -> pure s

-- | Ensure a Maybe has a value present.
just :: forall a. String -> Validation (Maybe a) a
just field = case _ of
    Nothing -> singleError field "A value is required."
    Just v  -> pure v

-- | Ensure the string parses into a decimal with a maximum of two places.
cents :: String -> Validation (Maybe String) Cents
cents field str = case join (map Decimal.fromString str) of
    Nothing -> singleError field "A decimal value is required."
    Just dec ->
        if maxDecimalPlaces 2 dec then
            pure $ decimalToCents dec
        else
            singleError field "A maximum of 2 decimal places is allowed."

optionalCents :: String -> Validation (Maybe String) (Maybe Cents)
optionalCents field str = case join (map Decimal.fromString str) of
    Nothing -> pure Nothing
    Just dec ->
        if maxDecimalPlaces 2 dec then
            pure $ Just $ decimalToCents dec
        else
            singleError field "A maximum of 2 decimal places is allowed."

optionalPercentage :: String -> Validation (Maybe String) (Maybe Percentage)
optionalPercentage field str = case join (map Decimal.fromString str) of
    Nothing -> pure Nothing
    Just dec ->
        if maxDecimalPlaces 2 dec then
            pure $ Just $ decimalToPercentage dec
        else
            singleError field "A maximum of 2 decimal places is allowed."

-- | Ensure the decimal value has the given number of decimal places at most.
maxDecimalPlaces :: Int -> Decimal.Decimal -> Boolean
maxDecimalPlaces places decimal =
    Just decimal == Decimal.fromString (Decimal.toFixed places decimal)


-- | Ensure the string can parse into a Date.
utcDate :: forall m. DateTime m => String -> String -> m (V.V FormErrors UTCDate)
utcDate field str =
    parseDate str >>= case _ of
        Nothing ->
            pure $ singleError field "A valid date is required."
        Just date ->
            pure $ pure $ UTCDate date

toEither :: forall a. V.V FormErrors a -> Either FormErrors a
toEither = V.toEither

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


-- Response Validation

-- | Decode the response, returning the result or any errors. If the response's
-- | status code is `422`, we decode `FormErrors` from the response body
-- | instead of `a`.
handleResponseErrors :: forall a
    . DecodeJson a
   => Response (Either ResponseFormatError String)
   -> Response (Either FormErrors a)
handleResponseErrors resp =
    mapBody $ collapse >=>
        if resp.status == StatusCode 422
        then decodeFailure
        else decodeSuccess
  where
    mapBody :: forall b. (Either ResponseFormatError String -> b) -> Response b
    mapBody f =
        resp { body = f resp.body }

    collapse :: forall b. Either ResponseFormatError b -> Either FormErrors b
    collapse =
        lmap $ singleton "" <<< printResponseFormatError

    decodeFailure :: forall b. String -> Either FormErrors b
    decodeFailure =
        runDecoder decodeErrors >=> Left

    decodeSuccess :: forall b. DecodeJson b => String -> Either FormErrors b
    decodeSuccess =
        runDecoder decodeJson

    runDecoder :: forall b. (Json -> Either String b) -> String -> Either FormErrors b
    runDecoder decoder = (jsonParser >=> decoder) >>> lmap (singleton "")

-- | Decode `FormErrors` from an object with an `errors` field.
decodeErrors :: Json -> Either String FormErrors
decodeErrors json = do
    errors <- decodeJson json >>= (_ .: "errors")
    let (arr :: Array (Tuple String Json)) = Object.toUnfoldable errors
    M.fromFoldable <$> traverse decodeValues arr
  where
    decodeValues :: forall a. DecodeJson a
       => Tuple String Json -> Either String (Tuple String a)
    decodeValues (Tuple key val) =
        Tuple key <$> decodeJson val
