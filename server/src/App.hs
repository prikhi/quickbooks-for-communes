{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module App where

import Api (api)
import Data.Text (Text)
import Network.Wai (Application)
import QBWC (QWCConfig(..), QBType(..), Schedule(..))
import QBXML (Callback(..), CallbackResponse(..))
import Servant.API ((:<|>)(..), NoContent(..))
import Servant.Server (Handler, serve)

app :: Application
app = serve api server

server :: Handler (QWCConfig, Text) :<|> Handler NoContent :<|> (Callback -> Handler CallbackResponse)
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

certRoute :: Handler NoContent
certRoute = return NoContent

generateQwc :: Handler (QWCConfig, Text)
generateQwc = return (qwcConfig, "acc-sync")

accountQuery :: Callback -> Handler CallbackResponse
accountQuery r = case r of
    ServerVersion ->
        return $ ServerVersionResp "0.1.0.0"
