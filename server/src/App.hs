{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module App
    ( app
    , AppEnv(..)
    )
where

import           Api                            ( API
                                                , api
                                                , QWCFile(..)
                                                , NewCompany(..)
                                                )
import           Config                         ( AppConfig(..) )
import           Control.Exception.Safe         ( try )
import           Control.Monad                  ( (>=>) )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Except           ( ExceptT(..) )
import           Control.Monad.Reader           ( MonadReader
                                                , runReaderT
                                                , asks
                                                )
import           Crypto.BCrypt                  ( hashPasswordUsingPolicy
                                                , slowerBcryptHashingPolicy
                                                )
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( pack )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8
                                                , decodeUtf8
                                                )
import           Data.UUID                      ( UUID )
import           Data.Version                   ( showVersion )
import           Database.Persist.Sql           ( insert_
                                                , getBy
                                                )
import           DB.Schema                      ( Session(..)
                                                , Unique(..)
                                                , Company(..)
                                                )
import           DB.Fields                      ( UUIDField(..)
                                                , SessionType(..)
                                                , SessionStatus(..)
                                                , SessionError(..)
                                                )
import           Network.Wai                    ( Application )
import           Paths_qbfc                     ( version )
import           QuickBooks.QBXML               ( Request(AccountQuery) )
import           QuickBooks.WebConnector        ( QWCConfig(..)
                                                , QBType(..)
                                                , Schedule(..)
                                                , Callback(..)
                                                , CallbackResponse(..)
                                                , AuthResult(..)
                                                , Username(..)
                                                , Password(..)
                                                )
import           Servant.API
import           Servant.Server                 ( ServerT
                                                , Server
                                                , Handler(..)
                                                , serve
                                                , hoistServer
                                                )
import           System.Random                  ( randomIO )
import           Types                          ( AppEnv(..)
                                                , AppM(..)
                                                , AppSqlM
                                                , runDB
                                                )
import qualified Validation                    as V

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
server =
    (newCompany :<|> generateAccountSyncQwc) :<|> (certRoute :<|> accountQuery)


-- | TODO: Error throwing on unhashable password & uniqueness violations
newCompany :: NewCompany -> AppM QWCFile
newCompany = V.validateOrThrow >=> \NewCompany {..} -> do
    hashedPassword <-
        liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy $ encodeUtf8
            ncPassword
    cfg <- asks appConfig
    case hashedPassword of
        Nothing -> V.validationError $ V.formError
            "There was an issue securing the password. Please try again."
        Just pass -> runDB $ do
            existingName <- nameError isNothing
                <$> getBy (UniqueCompanyName ncName)
            existingUser <- userError_ isNothing
                <$> getBy (UniqueCompanyUser ncUsername)
            let uniquenessTest = (,) <$> existingUser <*> existingName
            V.whenValid uniquenessTest $ \_ -> do
                let company :: Company = Company
                        { companyName         = ncName
                        , companyUser         = ncUsername
                        , companyPassword     = decodeUtf8 pass
                        , companyFileName     = Nothing
                        , companyLastSyncTime = Nothing
                        }
                insert_ company
                return $ companyQwcConfig cfg company
  where
    nameError = V.validate "name" "A company with this name already exists."
    userError_ =
        V.validate "username" "A company is already using this username."


-- | TODO: merge user field into QWCConfig record?
-- TODO: add appname & api base to appconfig
companyQwcConfig :: AppConfig -> Company -> QWCFile
companyQwcConfig cfg c = QWCFile
    ( QWCConfig
        { qcAppDescription     = "Syncing Accounts to QBFC"
        , qcAppDisplayName     = Nothing
        , qcAppID              = "QBFC_AS"
        , qcAppName            = "QuickBooks For Communes - " <> companyName c
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
        , qcScheduler          = Just $ EveryMinute $ appAccountSyncInterval cfg
        , qcStyle              = Nothing
        , qcUnattendedModePref = Nothing
        }
    , companyUser c
    )
  where
    url path = T.concat
        ["https://", appHostname cfg, ":", pack (show $ appPort cfg), path]


-- | The QuickBooks WebConnector Configuration for Account Syncing.
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
--
-- TODO: The @encoding@ attribute in the XML declaration makes the
-- WebConnector not accept the given file. Should try to fix this
-- somehow... Maybe directly return the rendered & processed ByteString
-- from this route instead of XML?
--
-- TODO: Take a CompanyId as a parameter & correctly generate their file!
generateAccountSyncQwc :: MonadReader AppEnv m => m QWCFile
generateAccountSyncQwc = do
    cfg <- asks appConfig
    return $ QWCFile (accountSyncQwcConfig cfg, appAccountSyncUsername cfg)

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
        -- TODO: pass companyfile if known?
        -- TODO: Use bcrypt to hash password & compare to stored hash
        expectedUser <- asks $ appAccountSyncUsername . appConfig
        expectedPass <- asks $ appAccountSyncPassword . appConfig
        let (authResult, status, authError) =
                if expectedUser /= user || expectedPass /= pass
                    then (InvalidUser, Completed, Just InvalidAuthentication)
                    else (ValidUser, Initiated, Nothing)
        ticket <- runDB $ do
            ticket <- generateUniqueTicket
            insert_ Session
                { sessionTicket = UUIDField ticket
                , sessionType   = AccountSync
                , sessionStatus = status
                , sessionError  = authError
                }
            return ticket
        return $ AuthenticateResp ticket authResult Nothing Nothing
    InitialSendRequestXML{} -> do
        -- TODO: Update company file name?
        --       Build query w/ modified time as globalLastSyncTime
        liftIO $ print r
        return $ SendRequestXMLResp AccountQuery
    ReceiveResponseXML _ resp -> do
        -- TODO: Update Accounts for company
        liftIO $ print resp
        return $ ReceiveResponseXMLResp 100

-- | Generate a 'Session' 'UUID' that does not yet exist in the
-- database.
generateUniqueTicket :: AppSqlM UUID
generateUniqueTicket = do
    ticket <- liftIO randomIO
    getBy (UniqueTicket $ UUIDField ticket) >>= \case
        Nothing -> return ticket
        Just _  -> generateUniqueTicket
