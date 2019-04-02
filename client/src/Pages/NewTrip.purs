module Pages.NewTrip
    ( component
    , Query(..)
    ) where

import Prelude

import Data.Maybe (Maybe(Nothing))
import Halogen as H
import Halogen.HTML as HH

component :: forall i o m
    . H.Component HH.HTML Query i o m
component =
    H.component
        { initialState: const unit
        , render
        , eval
        , receiver: const Nothing
        }

data Query a
    = NoOp a

eval :: forall o m. Query ~> H.ComponentDSL Unit Query o m
eval = case _ of
    NoOp next ->
        pure next


render :: Unit -> H.ComponentHTML Query
render _ =
    HH.div_ [HH.text "Hello World"]
