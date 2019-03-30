{-# LANGUAGE TypeOperators #-}
{- | Expose the API as a WAI 'Application'.
-}
module App
    ( app
    , AppEnv(..)
    , server
    )
where

import           Api                            ( API
                                                , api
                                                )
import           Control.Exception.Safe         ( try )
import           Control.Monad.Except           ( ExceptT(..) )
import           Control.Monad.Reader           ( runReaderT )
import           Network.Wai                    ( Application )
import qualified Routes.Frontend               as Frontend
import qualified Routes.QuickBooks             as QuickBooks
import           Servant.API
import           Servant.Server                 ( ServerT
                                                , Server
                                                , Handler(..)
                                                , serve
                                                , hoistServer
                                                )
import           Types                          ( AppEnv(..)
                                                , AppM(..)
                                                )


-- | The API server as a WAI Application.
app :: AppEnv -> Application
app env = serve api appServer
  where
    appServer :: Server API
    appServer = hoistServer api transform server
    -- Run the AppM Route, Catch IO Errors, & Construct a Handler from the
    -- Result.
    transform :: AppM a -> Handler a
    transform m = Handler $ ExceptT $ try $ runReaderT (fromAppM m) env


-- | Join the separate route handlers to create our API.
server :: ServerT API AppM
server = Frontend.routes :<|> QuickBooks.routes
