module Main where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.PushState (makeInterface, matches)

import App (AppEnv(..), runAppM)
import Router (Route, Query(..), router, component)

main :: Effect Unit
main = do
    nav <- makeInterface
    let env = Env { nav }
    HA.runHalogenAff do
        body <- HA.awaitBody
        let app = H.hoist (flip runAppM env) component
        driver <- runUI app unit body
        liftEffect <<< void $ matches router (handlePathChange driver) nav
  where
    handlePathChange :: forall o
        . H.HalogenIO Query o Aff -> Maybe Route -> Route -> Effect Unit
    handlePathChange driver _ route = do
        logShow route
        launchAff_ $ driver.query $ H.tell $ UpdateRoute route
