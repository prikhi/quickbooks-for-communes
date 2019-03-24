{-# LANGUAGE OverloadedStrings #-}
{- | Validation of API Types

TODO: Field Name Newtype? With IsString Instance? Semigroup for nesting?
         * withIndex :: FieldName -> Integer -> FieldName
         * Prevent creation of empty names except formError function?

If you define an 'AppValidation' for an API type that is POSTed to the
server, you can run 'validateOrThrow` in a Servant route to throw a 422
error with the errors in an @errors@ field.

Error messages are categorized by field name, or for the type as a whole.
The 'FormErrors' type has a 'Semigroup' instance that merges the lists of
field errors, removing any duplicates, allowing the use of the
'Data.Validation' module for accumulating multiple errors during valiation.

You can also

-}
module Validation
    ( -- * Running
      AppValidation(..)
    , validateOrThrow
    , validationError
    , whenValid
      -- * Validators
    , isNonEmpty
    , validate
      -- * Errors
    , FieldName
    , Message
    , FormErrors
    , fromFormErrors
    , null
    , singleton
    , formError
    )
where

import           Prelude                 hiding ( null )

import           Control.Monad.Catch            ( MonadThrow
                                                , throwM
                                                )
import           Data.Aeson                     ( (.=)
                                                , ToJSON(..)
                                                , object
                                                , encode
                                                )
import qualified Data.HashMap.Strict           as HM
import qualified Data.List                     as L
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Validation               as V
import           Servant.Server                 ( ServantErr(errBody)
                                                , err422
                                                )

-- | Provide a standard validation for a type.
--
-- Usually defined by API-specific types that are valiated in Handler
-- routes. A 422 error will be thrown when a value does not pass valiation.
class AppValidation a where
    -- | Produce a 'Validation' for a type
    validator :: a -> V.Validation FormErrors a

-- | Validate the value or throw a validation error.
validateOrThrow :: (MonadThrow m, AppValidation a) => a -> m a
validateOrThrow a = case validator a of
    V.Failure errs -> validationError errs >> return a
    V.Success a_   -> return a_

-- | Run an action when valid, or throw the 422 error.
whenValid :: MonadThrow m => V.Validation FormErrors a -> (a -> m ()) -> m ()
whenValid validation onSuccess = case validation of
    V.Failure errs -> validationError errs
    V.Success a_   -> onSuccess a_




-- | Errors out if the field's text is empty.
isNonEmpty :: FieldName -> Text -> V.Validation FormErrors Text
isNonEmpty field = validate field "This field is required." (/= "")

-- | Test some invariant against a value, assigning a field & message to
-- the error.
validate
    :: FieldName -> Message -> (a -> Bool) -> a -> V.Validation FormErrors a
validate field message = V.validate (singleton field message)


-- | Field identifiers.
type FieldName = Text

-- | An error message.
type Message = Text

-- | Potential validation errors, mapping from field names to error
-- messages.
--
-- General errors with the form are represented with an empty(@""@) field name.
newtype FormErrors
    = FormErrors
        { fromFormErrors :: HM.HashMap FieldName [Message]
        } deriving (Show, Read)

-- | Append errors by merging the sets of messages for each field.
instance Semigroup FormErrors where
    (FormErrors map1) <> (FormErrors map2) = FormErrors $
        HM.unionWith (\l1 l2 -> L.nub $ l1 <> l2) map1 map2

-- | An empty set of errors.
instance Monoid FormErrors where
    mempty = FormErrors mempty

-- | Convert the FormErrors into an object of Field Names to arrays of
-- error messages, wrapping it as the @errors@ field of an otherwise empty
-- object.
--
-- @
-- { "errors": { "field1": ["err1", "err2"], "otherField" : ["err3"] } }
-- @
instance ToJSON FormErrors where
    toJSON (FormErrors errs) =
        object [ "errors" .= errs ]

-- | Is the error set empty?
null :: FormErrors -> Bool
null (FormErrors errMap) =
    let emptyMap      = HM.null errMap
        emptyContents = HM.null $ HM.filterWithKey
            (\name errors -> not (T.null name) && L.any (/= "") errors)
            errMap
    in  emptyMap || emptyContents

-- | Throw a 422 servant error if the errors are nonempty. The body of the
-- message contains the errors encoded as JSON.
validationError :: MonadThrow m => FormErrors -> m ()
validationError errs =
    if null errs then return () else throwM $ err422 { errBody = encode errs }

-- | Create an error set from a field name and an error message.
singleton :: FieldName -> Message -> FormErrors
singleton field message = FormErrors $ HM.singleton field [message]

-- | Create an error that's not field-specific.
formError :: Message -> FormErrors
formError = singleton ""
