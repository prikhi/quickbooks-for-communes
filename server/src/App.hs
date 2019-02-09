{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module App where

import Api (api)
import Data.ByteString (ByteString)
import QBWC (QWCConfig(..), QBType(..), Schedule(..), generateConnectorFile)
import Servant.API ((:<|>)(..), NoContent(..))
import Servant.Server (Handler, serve)
import Network.Wai (Application)

app :: Application
app = serve api server

server :: Handler ByteString :<|> Handler NoContent
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

generateQwc :: Handler ByteString
generateQwc = return $ generateConnectorFile qwcConfig "acc-sync"

accountQuery :: Handler NoContent
accountQuery = return NoContent
