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
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup as V
import Foreign.Object as Object
import Halogen as H


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


-- | Ensure a Maybe String has a non-empty value present.
validateNonEmpty :: String -> Maybe String -> V.V FormErrors String
validateNonEmpty field = case _ of
    Nothing -> singleError field "A value is required."
    Just "" -> singleError field "A value is required."
    Just s  -> pure s

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
