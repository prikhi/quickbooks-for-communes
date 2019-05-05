{- | Functions for rendering form labels, inputs, buttons, & errors.
-}
module Forms
    ( formErrors
    , button
    , submitButton
    , input
    , optionalInput
    , dateInput
    , dollarInput
    , optionalValue
    , labelWrapper
    ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE


-- | Render the general errors for a form.
formErrors :: forall p i. Array String -> HH.HTML p i
formErrors errs =
    if not $ Array.null errs then
        HH.p [HP.class_ $ H.ClassName "form-errors"]
            [ HH.text "We encountered the following errors when processing \
                \your request. Please address them and try re-submitting the form:"
            , HH.ul_ $ map (\err -> HH.li_ [HH.text err]) errs
            ]
    else
        HH.text ""

-- Buttons

-- | Render a standard `HH.button` element.
button :: forall p i. String -> HP.ButtonType -> H.ClassName -> i -> HH.HTML p i
button label type_ class_ action =
    HH.button
        [ HE.onClick $ const $ Just action, HP.class_ class_, HP.type_ type_ ]
        [ HH.text label ]

-- | Render a button input to submit a form. Uses the primary button styling.
submitButton :: forall p i. String -> HH.HTML p i
submitButton label =
    HH.button
        [ HP.class_ (H.ClassName "primary"), HP.type_ HP.ButtonSubmit ]
        [ HH.text label ]


-- Inputs

-- | An input for required dates.
dateInput :: forall p i
    . String
   -- ^ Label Text
   -> Maybe String
   -- ^ Value
   -> (String -> i)
   -- ^ onChange Action
   -> Array String
   -- ^ Errors
   -> String
   -- ^ Help Text
   -> HH.HTML p i
dateInput label value action errors helpText =
    labelWrapper label errors helpText $
        HH.input $
            [ HP.type_ HP.InputDate
            , HP.required true
            , HE.onValueChange $ Just <<< action
            ] <> optionalValue value


-- | An input for zero or positive dollar amounts with a step size of 1 cent.
dollarInput :: forall p i
    . String
   -- ^ Label text
   -> Maybe String
   -- ^ Value
   -> (String -> i)
   -- ^ onChange Action
   -> Array String
   -- ^ Errors
   -> String
   -- ^ Help text
   -> HH.HTML p i
dollarInput label value action errors helpText =
    labelWrapper label errors helpText $
        HH.input $
            [ HP.type_ HP.InputNumber
            , HP.required true
            , HE.onValueInput $ Just <<< action
            , HP.min 0.0
            , HP.step $ HP.Step 0.01
            ] <> optionalValue value


-- | The Label, Help Text, & Error wrapper around an `HH.input` element.
labelWrapper :: forall p i
    . String -> Array String -> String -> HH.HTML p i -> HH.HTML p i
labelWrapper label errors helpText inputElement =
    HH.label errorClass
        [ HH.div_ $
            [ HH.text label ]
            <> helpElement
        , inputElement
        , if hasError
              then HH.ul_ $ map (\e -> HH.li_ [HH.text e]) errors
              else HH.text ""
        ]
  where
    helpElement :: forall p_ i_. Array (HH.HTML p_ i_)
    helpElement =
        if helpText /= "" then [ HH.p_ [ HH.text helpText ] ] else []
    hasError :: Boolean
    hasError =
        not $ Array.null errors

    errorClass :: forall r i_. Array (HP.IProp ( class :: String | r ) i_)
    errorClass =
        if hasError
            then [HP.class_ $ H.ClassName "error"]
            else []

-- | Render a required text `HH.input` element with a label, help text, and an
-- | error list.
input :: forall p i
    . String
   -- ^ Label Text
   -> HP.InputType
   -- ^ input `type`
   -> Maybe String
   -- ^ Value
   -> (String -> i)
   -- ^ onChange action
   -> Array String
   -- ^ Errors
   -> String
   -- ^ Help text
   -> HH.HTML p i
input label type_ value action errors helpText =
    labelWrapper label errors helpText $
        HH.input $
            [ HP.type_ type_
            , HP.required true
            , HE.onValueInput $ Just <<< action
            ] <> optionalValue value

-- | Render an optional HH.input element. TODO: Refactor this & input functions.
optionalInput :: forall p i
    . String
   -- ^ Label Text
   -> HP.InputType
   -- ^ input `type`
   -> Maybe String
   -- ^ Value
   -> (String -> i)
   -- ^ onChange action
   -> Array String
   -- ^ Errors
   -> String
   -- ^ Help text
   -> HH.HTML p i
optionalInput label type_ value action errors helpText =
    labelWrapper label errors helpText $
        HH.input $
            [ HP.type_ type_
            , HE.onValueInput $ Just <<< action
            ] <> optionalValue value

-- | Make a `value` attribute for an input element, if a string is present.
optionalValue :: forall r i_. Maybe String -> Array (HP.IProp ( value :: String | r ) i_)
optionalValue = case _ of
    Nothing ->
        []
    Just val -> [ HP.value val ]
