{- | A component for rendering Account inputs with autocompleting dropdown
menus.

You feed the component an array of accounts and it'll notify the parent
component when an account has been selected, or the selection has been cleared.

There's also a message for informing parents when the enter key is pressed
while the dropdown is closed. We use this along with the Focus query when
rendering selects in tables, allowing parents to focus the select in the next
row of the table.


TODO:
    * Fuzzy search (purescript-fuzzy package?)
    * Highlight search matches
    * Render the description & parent(or root) account or the account type?

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
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.String as String
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import NSelect as Select
import Web.UIEvent.KeyboardEvent as KE

import App
    ( class SelectComponent, selectComponent
    , class PreventDefaultEnter, preventEnter
    )
import Server (AccountData(..))


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


type State =
    { selected :: Maybe AccountData
    , value :: String
    , items :: Array AccountData
    , filteredItems :: Array AccountData
    }

initialState :: Array AccountData -> State
initialState items =
    { selected: Nothing
    , value: ""
    , items
    , filteredItems: items
    }


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
               , filteredItems = accounts
               , selected = newSelected
               , value = newValue
               })
    HandleSelect selectMsg -> case selectMsg of
        Select.Selected index -> do
            -- Set the new item & input value, inform the parent of the
            -- selection, & close the dropdown menu.
            st <- H.get
            for_ (Array.index st.filteredItems index) \item@(AccountData a) -> do
                H.modify_ $ _ { selected = Just item, value = a.name }
                H.raise $ Selected item
            void $ H.query _select unit Select.close
        Select.InputValueChanged value -> do
            -- Set the new input value & filter the item list using the new
            -- value.
            H.modify_ $ \state -> state
                { value = value
                , filteredItems = filterAccounts value state.items
                }
            void $ H.query _select unit $ Select.highlight 0
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
                { selected = Nothing, value = "", filteredItems = state.items }
            void $ H.query _select unit Select.open
            H.raise Cleared
  where
    filterAccounts :: String -> Array AccountData -> Array AccountData
    filterAccounts searchValue =
        Array.filter \(AccountData a) ->
            String.contains (String.Pattern searchValue) a.name


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

renderSelect :: forall m. State -> Select.State -> Select.HTML Action () m
renderSelect state st =
    HH.div (Select.setRootProps [ HP.class_ $ H.ClassName "account-select" ])
        $ [ renderInput st.isOpen
          , renderDropdown
          ]
  where
    renderInput :: forall m_. Boolean -> Select.HTML Action () m_
    renderInput isOpen =
        HH.input
            $ Select.setInputProps'
                { onKeyDown: HandleKeypress isOpen }
                [ HP.value state.value ]
    renderDropdown :: forall m_. Select.HTML Action () m_
    renderDropdown =
        let visibleAttr =
                guard st.isOpen [ HP.class_ $ H.ClassName "opened" ]
        in  HH.div (Select.setMenuProps visibleAttr)
                $ Array.mapWithIndex renderItem state.filteredItems
    renderItem :: forall m_. Int -> AccountData -> Select.HTML Action () m_
    renderItem index (AccountData account) =
        let selectedAttr =
                guard (index == st.highlightedIndex)
                    [ HP.class_ $ H.ClassName "selected" ]
        in  HH.div (Select.setItemProps index selectedAttr)
                [ HH.text account.name ]
