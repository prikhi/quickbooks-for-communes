{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module App where

import Api (api)
import Data.Text (Text)
import Network.Wai (Application)
import QBWC (QWCConfig(..), QBType(..), Schedule(..))
import QBXML (Callback(..), CallbackResponse(..))
import Servant.API ((:<|>)(..) )
import Servant.Server (Handler, serve)

app :: Application
app = serve api server

server :: Handler (QWCConfig, Text) :<|> (Callback -> Handler CallbackResponse)
server = generateQwc
    :<|> accountQuery

qwcConfig :: QWCConfig
qwcConfig =
    QWCConfig
        { qcAppDescription = "Syncing Accounts to QBFC"
        , qcAppDisplayName = Nothing
        , qcAppID = "QBFC_AS"
        , qcAppName = "QuickBooks For Communes - Account Sync"
        , qcAppSupport = "https://ada.acorn:3000/support/"
        , qcAppUniqueName = Nothing
        , qcAppURL = "https://ada.acorn:3000/accountSync/"
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

generateQwc :: Handler (QWCConfig, Text)
generateQwc = return (qwcConfig, "acc-sync")

accountQuery :: Callback -> Handler CallbackResponse
accountQuery r = case r of
    ServerVersion ->
        return $ Version "0.1.0.0"
