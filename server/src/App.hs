{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module App where

import Api (api)
import Data.Text (Text, pack)
import Data.Version (showVersion)
import Network.Wai (Application)
import Paths_qbfc (version)
import QuickBooks.WebConnector (QWCConfig(..), QBType(..), Schedule(..), Callback(..), CallbackResponse(..))
import Servant.API ((:<|>)(..), NoContent(..))
import Servant.Server (Handler, serve)

-- | The API server as a WAI Application.
app :: Application
app = serve api server

-- | Represents the Route Handlers for our server.
type Routes =
         Handler (QWCConfig, Text)
    :<|> Handler NoContent
    :<|> (Callback -> Handler CallbackResponse)

-- | Join the separate route handlers to create our API.
server :: Routes
server = generateQwc
    :<|> certRoute
    :<|> accountQuery


-- | The QuickBooks WebConnector Configuration for Account Syncing.
--
-- TODO: Populate the URLs & IDs from environmental/config variables.
qwcConfig :: QWCConfig
qwcConfig =
    QWCConfig
        { qcAppDescription = "Syncing Accounts to QBFC"
        , qcAppDisplayName = Nothing
        , qcAppID = "QBFC_AS"
        , qcAppName = "QuickBooks For Communes - Account Sync"
        , qcAppSupport = "https://lucy.acorn:3000/support/"
        , qcAppUniqueName = Nothing
        , qcAppURL = "https://lucy.acorn:3000/accountSync/"
        , qcCertURL = Just "https://lucy.acorn:3000/cert/"
        , qcAuthFlags = []
        , qcFileID = read "bb62c0ae-3a4b-464c-bbf0-39acf68512c7"
        , qcIsReadOnly = False
        , qcNotify = False
        , qcOwnerID = read "bb62c0ae-3a4b-464c-bbf0-39acf68512c7"
        , qcPersonalDataPref = Nothing
        , qcQBType = Financial
        , qcScheduler = Just (EveryMinute 1)
        , qcStyle = Nothing
        , qcUnattendedModePref = Nothing
        }

-- | An empty route that does absolutely nothing. This is used for SSL
-- certificate verification by the WebConnector.
certRoute :: Handler NoContent
certRoute = return NoContent

-- | Generate a QWC File for the @/accountSync/@ route using the
-- "qwcConfig" & the @acc-sync@ Username.
generateQwc :: Handler (QWCConfig, Text)
generateQwc = return (qwcConfig, "acc-sync")

-- | Perform querying/syncing operations for the QuickBooks Accounts.
accountQuery :: Callback -> Handler CallbackResponse
accountQuery r = case r of
    ServerVersion ->
        return . ServerVersionResp . pack $ showVersion version
    ClientVersion _ ->
        return $ ClientVersionResp ""
