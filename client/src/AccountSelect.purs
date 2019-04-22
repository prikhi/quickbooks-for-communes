{- | A component for rendering Account inputs with autocompleting dropdown
menus.

You feed the component an array of accounts and it'll notify the parent
component when an account has been selected, or the selection has been cleared.

There's also a message for informing parents when the enter key is pressed
while the dropdown is closed. We use this along with the Focus query when
rendering selects in tables, allowing parents to focus the select in the next
row of the table.

-}
module AccountSelect
    ( component
    , Slot
    , Query(..)
    , Message(..)
    )
    where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Fuzzy (Fuzzy(..), Segments, match)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Rational ((%))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import NSelect as Select
import Web.UIEvent.KeyboardEvent as KE

import App
    ( class SelectComponent, selectComponent
    , class PreventDefaultEnter, preventEnter
    )
import Server (AccountData(..), prettyAccountType)


-- | An autocompleting input with a dropdown menu for selecting an Account.
-- | Takes an Array of AccountData as input and raises a Message when one is
-- | selected.
component :: forall m
    . SelectComponent m
   => PreventDefaultEnter m
   => H.Component HH.HTML Query (Array AccountData) Message m
component = H.mkComponent
    { render
    , initialState
    , eval: H.mkEval $ H.defaultEval
        { handleAction = evalAction
        , handleQuery = evalQuery
        , receive = Just <<< Receive
        }
    }

-- | The component's Slot type. Use this when defining the parent component's
-- | ChildSlots.
type Slot
    = H.Slot Query Message

-- | Available messages a parent can send to this component.
data Query a
    = Focus a
    -- ^ Focus the input field

-- | Messages the component can send to it's parent.
data Message
    = Selected AccountData
    -- ^ An account was selected
    | Cleared
    -- ^ The account was unselected
    | EnterKeyDownWhileClosed KE.KeyboardEvent
    -- ^ The enter key was pressed while the dropdown was closed.
    -- This is useful for selects in a table. When receiving this message,
    -- you can move to the first input in the next row of the table.


-- | The internal state of the component.
type State =
    { selected :: Maybe AccountData
    -- ^  Currently selected Account
    , value :: String
    -- ^ Current value of the search input
    , items :: Array AccountData
    -- ^ Available items to choose from
    , filteredItems :: Array SelectItem
    -- ^ Items currently filtered by the search string
    }

-- | AccountData that has potentially passed through search matching. Built
-- | from the State.items and used for rendering the dropdown items.
type SelectItem =
    { account :: AccountData
    , match :: Maybe (Fuzzy AccountData)
    }

-- | Make an unmatched SelectItem from an Account.
pureItem :: AccountData -> SelectItem
pureItem account =
    { account
    , match: Nothing
    }

-- | An empty input with the given possible accounts.
initialState :: Array AccountData -> State
initialState items =
    { selected: Nothing
    , value: ""
    , items
    , filteredItems: map pureItem items
    }


-- | Each component contains a single NSelect component.
type ChildSlots =
    ( select :: Select.Slot Action Unit
    )

-- | Grab the child NSelect component
_select = SProxy :: SProxy "select"


data Action
    = HandleSelect (Select.Message Action)
    -- ^ Handle messages from the child NSelect
    | HandleKeypress Boolean KE.KeyboardEvent
    -- ^ Extend the keypress handling of the child NSelect
    | Receive (Array AccountData)
    -- ^ Handle receiving a new Array of Accounts as input from the parent.

-- | Handle the internal actions of the component.
evalAction :: forall m
    . PreventDefaultEnter m
   => Action
   -> H.HalogenM State Action ChildSlots Message m Unit
evalAction = case _ of
    Receive accounts -> do
        -- Replace the accounts - keeping the selected Account if it is in the
        -- new list.
        st <- H.get
        let newSelected = st.selected >>= \selected ->
                Array.find (\account -> account == selected) accounts
            newValue = maybe "" (\(AccountData a) -> a.name) newSelected
        H.modify_
            (_ { items = accounts
               , filteredItems = map pureItem accounts
               , selected = newSelected
               , value = newValue
               })
    HandleSelect selectMsg -> case selectMsg of
        Select.Selected index -> do
            -- Set the new item & input value, inform the parent of the
            -- selection, & close the dropdown menu.
            st <- H.get
            for_ (Array.index st.filteredItems index) \item -> do
                let a@(AccountData account) = item.account
                H.modify_ $ _ { selected = Just a, value = account.name }
                H.raise $ Selected a
            void $ H.query _select unit Select.close
        Select.InputValueChanged value -> do
            -- Set the new input value & filter the item list using the new
            -- value.
            H.modify_ $ \state -> state
                { value = value
                , filteredItems = filterAccounts value state.items
                }
            void $ H.query _select unit $ Select.highlight 0
            void $ H.query _select unit Select.open
        Select.VisibilityChanged open ->
            -- Ensure the highlighted item is visible by re-setting it as the
            -- highlighted item. This is necessary because we use height
            -- transitions for the dropdown's open/close functionality which
            -- breaks the ability of the Select component to automatically
            -- scroll the highlighted element into view when the dropdown is
            -- opened.
            when open
                $ H.query _select unit (H.request Select.getState) >>= traverse_ \st ->
                    void $ H.query _select unit $ Select.highlight st.highlightedIndex
        Select.Emit action ->
            -- Recursively call our action handler using the raised action.
            evalAction action
    HandleKeypress isOpen event -> do
        let isTabKey = KE.key event == "Tab"
            isEnterKey = KE.key event == "Enter"
            isNavKey = KE.key event `Array.elem` ["ArrowUp", "ArrowDown"]
            isEscKey = KE.key event == "Escape"
            isBackspace = KE.key event == "Backspace"
        -- Select the highlighted item when pressing Tab with the dropdown open.
        when (isTabKey && isOpen) $
            void $ H.query _select unit Select.select
        -- Open the dropdown when the arrow keys are pressed.
        when isNavKey $
            void $ H.query _select unit Select.open
        -- When Enter is pressed & the dropdown is open, prevent the default
        -- action so the form is not submitted. If the dropdown is closed,
        -- raise a message for the parent component.
        when isEnterKey $
            if isOpen
                then void $ preventEnter event
                else H.raise $ EnterKeyDownWhileClosed event
        -- Close the dropdown without selecting an Account when Esc is pressed.
        when isEscKey $
            void $ H.query _select unit Select.close
        -- When an item is selected & backspace is pressed, clear the selection
        -- & make sure the dropdown is open.
        st <- H.get
        when (isBackspace && st.selected /= Nothing) $ do
            H.modify_ \state -> state
                { selected = Nothing
                , value = ""
                , filteredItems = map pureItem state.items
                }
            void $ H.query _select unit Select.open
            H.raise Cleared
  where
    -- Return only the Accounts that match the search string.
    filterAccounts :: String -> Array AccountData -> Array SelectItem
    filterAccounts searchValue =
        if searchValue == "" then
            map pureItem
        else
            Array.mapMaybe (matchAccount searchValue)
                >>> Array.sortWith (_.match)
    -- Attempt to match an Account against some some string.
    matchAccount :: String -> AccountData -> Maybe SelectItem
    matchAccount searchValue i =
        let toObject (AccountData a) = Object.fromFoldable
                [ Tuple "name" a.name
                , Tuple "description" a.description
                , Tuple "type" $ prettyAccountType a.accountType
                ]
            fuzzyMatch =
                match true toObject searchValue i
        in  if aboveThreshold fuzzyMatch
            then Just { account: i, match: Just fuzzyMatch }
            else Nothing
    -- Allow matching only 11 out of every 12 characters
    aboveThreshold :: forall a. Fuzzy a -> Boolean
    aboveThreshold (Fuzzy match) =
        match.ratio >= (11 % 12)


-- | Handle requests from parent components.
evalQuery :: forall s o m a. Query a -> H.HalogenM s Action ChildSlots o m (Maybe a)
evalQuery = case _ of
    Focus next -> do
        -- Forward focus requests to the NSelect child component.
        void $ H.query _select unit Select.focus
        pure $ Just next



-- | Render the Account Select by simply rendering the NSelect child component.
render :: forall m
    . SelectComponent m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.slot _select unit selectComponent
    { render: renderSelect state
    , itemCount: Array.length state.filteredItems
    }
    $ Just <<< HandleSelect

-- | Render the NSelect component as a div containing an input and a dropdown
-- | div.
renderSelect :: forall m. State -> Select.State -> Select.HTML Action () m
renderSelect state st =
    HH.div (Select.setRootProps [ HP.class_ $ H.ClassName "account-select" ])
        $ [ renderInput st.isOpen
          , renderDropdown
          ]
  where
    -- Render the text input, raising the HandleKeypress message on keypresses.
    renderInput :: forall m_. Boolean -> Select.HTML Action () m_
    renderInput isOpen =
        HH.input
            $ Select.setInputProps'
                { onKeyDown: HandleKeypress isOpen }
                [ HP.value state.value ]
    -- Render the dropdown list of items
    renderDropdown :: forall m_ a. Select.HTML a () m_
    renderDropdown =
        let visibleAttr =
                guard st.isOpen [ HP.class_ $ H.ClassName "opened" ]
        in  HH.div (Select.setMenuProps visibleAttr)
                $ Array.mapWithIndex renderItem state.filteredItems
    -- Render an item, highlighting any matches and the entire item if seleted
    renderItem :: forall m_ a. Int -> SelectItem -> Select.HTML a () m_
    renderItem index selectItem =
        let selectedAttr =
                guard (index == st.highlightedIndex)
                    [ HP.class_ $ H.ClassName "selected" ]
        in  HH.div (Select.setItemProps index selectedAttr) $
                case selectItem.match of
                    Just fuzzyMatch ->
                        renderMatchingItem fuzzyMatch
                    Nothing ->
                        renderNormalItem selectItem.account
    -- Render an item without match data
    renderNormalItem :: forall m_ a. AccountData -> Array (Select.HTML a () m_)
    renderNormalItem a@(AccountData account) =
        let description =
                descriptionWrapper
                    [ pure $ typeWrapper
                        [ HH.text $ prettyAccountType account.accountType ]
                    , optionalDescription a
                        [ HH.text account.description ]
                    ]
        in  [ nameWrapper [ HH.text account.name ], description ]
    -- Render an item with Match data
    renderMatchingItem :: forall m_ a. Fuzzy AccountData -> Array (Select.HTML a () m_)
    renderMatchingItem (Fuzzy fuzzyMatch) =
        let (AccountData account) =
                fuzzyMatch.original
            segments =
                fuzzyMatch.segments
            nameHTML =
                renderSegments "name" account.name segments
            descriptionHTML =
                descriptionWrapper
                    [ pure typeHTML
                    , optionalDescription fuzzyMatch.original
                        $ renderSegments "description" account.description segments
                    ]
            typeHTML =
                typeWrapper
                    $ renderSegments "type" (prettyAccountType account.accountType)
                        segments
        in  [ nameWrapper nameHTML, descriptionHTML ]
    -- Wrap the account HTML in a div
    nameWrapper :: forall m_ a. Array (Select.HTML a () m_) -> Select.HTML a () m_
    nameWrapper =
        HH.div [ HP.class_ $ H.ClassName "account-name" ]
    -- Wrap the description HTML in a div, then an array
    descriptionWrapper :: forall m_ a
        . Array (Array (Select.HTML a () m_)) -> Select.HTML a () m_
    descriptionWrapper =
       Array.concat >>>  HH.div [ HP.class_ $ H.ClassName "account-description" ]
    -- Wrap the account type HTML in a span for styling
    typeWrapper :: forall m_ a. Array (Select.HTML a () m_) -> Select.HTML a () m_
    typeWrapper = HH.span [ HP.class_ $ H.ClassName "account-type" ]
    -- Show the description if non-empty, prefixed with a `: ` separator
    -- between the account type & description
    optionalDescription :: forall m_ a
        . AccountData -> Array(Select.HTML a () m_) -> Array (Select.HTML a () m_)
    optionalDescription (AccountData account) descriptionHTML =
        guard (account.description /= "")
            $ typeWrapper [ HH.text ": " ] `Array.cons` descriptionHTML
    -- Render the segments, given a field name & default value
    renderSegments :: forall m_ a
        . String -> String -> Object.Object Segments -> Array (Select.HTML a () m_)
    renderSegments field default segments =
        case Object.lookup field segments of
            Nothing ->
                [ HH.text default ]
            Just segs ->
                map renderSegment segs
    -- Render a matching segment wrapped in a span & an unmatched one as text
    renderSegment :: forall m_ a. Either String String -> Select.HTML a () m_
    renderSegment = case _ of
        Left str ->
            HH.text str
        Right matchStr ->
            HH.span [ HP.class_ $ H.ClassName "match" ]
                [ HH.text matchStr ]
