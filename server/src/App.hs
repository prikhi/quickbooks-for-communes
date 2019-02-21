{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module App
    ( app
    )
where

import           Api                            ( API
                                                , api
                                                )
import           Config                         ( AppConfig(..) )
import           Control.Exception.Safe         ( try )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Except           ( ExceptT(..) )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , runReaderT
                                                , ask
                                                , asks
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text                     as T
import           Data.Version                   ( showVersion )
import           Network.Wai                    ( Application )
import           Paths_qbfc                     ( version )
import           QuickBooks.WebConnector        ( QWCConfig(..)
                                                , QBType(..)
                                                , Schedule(..)
                                                , Callback(..)
                                                , CallbackResponse(..)
                                                , AuthResult(..)
                                                , Username(..)
                                                , Password(..)
                                                )
import           Servant.API                    ( (:<|>)((:<|>))
                                                , NoContent(..)
                                                )
import           Servant.Server                 ( ServerT
                                                , Server
                                                , Handler(..)
                                                , serve
                                                , hoistServer
                                                )
import           System.Random                  ( randomIO )

-- | The API server as a WAI Application.
app :: AppConfig -> Application
app cfg = serve api appServer
  where
    appServer :: Server API
    appServer = hoistServer api transform server
    -- Run the AppM Route, Catch IO Errors, & Construct a Handler from the
    -- Result.
    transform :: AppM a -> Handler a
    transform m = Handler $ ExceptT $ try $ runReaderT (fromAppM m) cfg

-- | The monadic stack the handler routes run under.
newtype AppM a
    = AppM { fromAppM :: ReaderT AppConfig IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig)


-- | Join the separate route handlers to create our API.
server :: ServerT API AppM
server = generateAccountSyncQwc :<|> certRoute :<|> accountQuery


-- | The QuickBooks WebConnector Configuration for Account Syncing.
--
-- TODO: Populate the URLs & IDs from environmental/config variables.
accountSyncQwcConfig :: AppConfig -> QWCConfig
accountSyncQwcConfig cfg
    = let
          url path =
              T.concat
                  [ "https://"
                  , appHostname cfg
                  , ":"
                  , pack (show $ appPort cfg)
                  , path
                  ]
      in  QWCConfig
              { qcAppDescription     = "Syncing Accounts to QBFC"
              , qcAppDisplayName     = Nothing
              , qcAppID              = "QBFC_AS"
              , qcAppName            = "QuickBooks For Communes - Account Sync"
              , qcAppSupport         = url "/support/"
              , qcAppUniqueName      = Nothing
              , qcAppURL             = url "/accountSync/"
              , qcCertURL            = Just $ url "/cert/"
              , qcAuthFlags          = []
              , qcFileID             = appAccountSyncID cfg
              , qcIsReadOnly         = False
              , qcNotify             = False
              , qcOwnerID            = appAccountSyncID cfg
              , qcPersonalDataPref   = Nothing
              , qcQBType             = Financial
              , qcScheduler = Just $ EveryMinute $ appAccountSyncInterval cfg
              , qcStyle              = Nothing
              , qcUnattendedModePref = Nothing
              }

-- | An empty route that does absolutely nothing. This is used for SSL
-- certificate verification by the WebConnector.
certRoute :: Monad m => m NoContent
certRoute = return NoContent

-- | Generate a QWC File for the @/accountSync/@ route using the
-- "qwcConfig" & the @acc-sync@ Username.
generateAccountSyncQwc :: MonadReader AppConfig m => m (QWCConfig, Text)
generateAccountSyncQwc = do
    cfg <- ask
    return (accountSyncQwcConfig cfg, appAccountSyncUsername cfg)

-- | Perform querying/syncing operations for the QuickBooks Accounts.
accountQuery :: Callback -> AppM CallbackResponse
accountQuery r = case r of
    ServerVersion ->
        -- Use the version specified in package.yaml
        return . ServerVersionResp . pack $ showVersion version
    ClientVersion _ ->
        -- Accept all Client versions
        return $ ClientVersionResp ""
    Authenticate (Username user) (Password pass) -> do
        -- Authentication succeeds when the User matches
        -- TODO: save ticket to database, pass companyfile if known
        -- TODO: Use bcrypt to hash password & compare to stored hash
        expectedUser <- asks appAccountSyncUsername
        expectedPass <- asks appAccountSyncPassword
        ticket       <- liftIO randomIO
        if expectedUser /= user || expectedPass /= pass
            then return $ AuthenticateResp ticket InvalidUser Nothing Nothing
            else return $ AuthenticateResp ticket ValidUser Nothing Nothing
