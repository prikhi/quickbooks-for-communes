{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
import           Control.Exception.Safe         ( Exception
                                                , MonadCatch
                                                , try
                                                , catch
                                                , throw
                                                )
import           Control.Monad                  ( (>=>)
                                                , unless
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Except           ( ExceptT(..) )
import           Control.Monad.Reader           ( runReaderT
                                                , asks
                                                )
import           Crypto.BCrypt                  ( hashPasswordUsingPolicy
                                                , slowerBcryptHashingPolicy
                                                , validatePassword
                                                )
import           Data.ByteString                ( ByteString )
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( pack )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8
                                                , decodeUtf8
                                                )
import           Data.UUID                      ( UUID )
import           Data.Version                   ( showVersion )
import           Database.Persist.Sql           ( (=.)
                                                , Entity(..)
                                                , insert
                                                , insert_
                                                , update
                                                , get
                                                , getBy
                                                )
import           DB.Schema                      ( Session(..)
                                                , SessionId
                                                , Company(..)
                                                , CompanyId
                                                , Unique(..)
                                                , EntityField(..)
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
                                                , err404
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
-- | TODO: Require user account via cookie auth
newCompany :: NewCompany -> AppM QWCFile
newCompany = V.validateOrThrow >=> \NewCompany {..} -> do
    hashedPassword <- hashPassword $ encodeUtf8 ncPassword
    cfg            <- asks appConfig
    case hashedPassword of
        Nothing -> V.validationError $ V.formError
            "There was an issue securing the password. Please try again."
        Just pass -> runDB $ do
            existingName <- nameError <$> getBy (UniqueCompanyName ncName)
            existingUser <- userError_ <$> getBy (UniqueCompanyUser ncUsername)
            let uniquenessTest = (,) <$> existingUser <*> existingName
            V.whenValid uniquenessTest $ \_ -> do
                let company :: Company = Company
                        { companyName         = ncName
                        , companyUser         = ncUsername
                        , companyPassword     = pass
                        , companyFileName     = Nothing
                        , companyLastSyncTime = Nothing
                        }
                insert_ company
                return $ companyQwcConfig cfg company
  where
    nameError =
        V.validate "name" "A company with this name already exists." isNothing
    userError_ = V.validate "username"
                            "A company is already using this username."
                            isNothing
    hashPassword :: MonadIO m => ByteString -> m (Maybe T.Text)
    hashPassword pass = fmap decodeUtf8
        <$> liftIO (hashPasswordUsingPolicy slowerBcryptHashingPolicy pass)


-- | Build the WebConnetor configuration file for a company.
-- TODO: merge user field into QWCConfig record?
-- TODO: add appname & api base url to appconfig
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


-- | Generate a QWC File for a 'Company'.
generateAccountSyncQwc :: CompanyId -> AppM QWCFile
generateAccountSyncQwc companyId = do
    cfg     <- asks appConfig
    company <- runDB $ get companyId >>= maybe (throw err404) return
    return $ companyQwcConfig cfg company


-- | An empty route that does absolutely nothing. This is used for SSL
-- certificate verification by the WebConnector.
certRoute :: Monad m => m NoContent
certRoute = return NoContent


-- | Perform querying/syncing operations for the QuickBooks Accounts.
--
-- TODO: Do both accounting pulling & entry pushing here?
accountQuery :: Callback -> AppM CallbackResponse
accountQuery r = case r of
    ServerVersion ->
        -- Use the version specified in package.yaml
        return . ServerVersionResp . pack $ showVersion version
    ClientVersion _ ->
        -- Accept all Client versions
        return $ ClientVersionResp ""
    Authenticate (Username user) (Password pass) -> do
        (ticket, sessionId) <- newSession
        authResult <- runDB $ flip catch (handleAuthFailure sessionId) $ do
            (Entity companyId company) <-
                getBy (UniqueCompanyUser user) >>= maybe authFailure return
            unless (isValidPassword company pass) authFailure
            update
                sessionId
                [ SessionStatus_ =. Authenticated
                , SessionCompany =. Just companyId
                ]
            case companyFileName company of
                Nothing       -> return ValidUser
                Just filename -> return $ CompanyFile filename
        return $ AuthenticateResp ticket authResult Nothing Nothing
    InitialSendRequestXML{} -> do
        -- TODO:
        --  * Error out if ticket has no company
        --  * Update company file name if Nothing.
        --  * Build query w/ modified time as globalLastSyncTime
        liftIO $ print r
        return $ SendRequestXMLResp AccountQuery
    ReceiveResponseXML _ resp -> do
        -- TODO: Update all Accounts for the company.
        -- Test equality by edit sequence?
        liftIO $ print resp
        return $ ReceiveResponseXMLResp 100
  where
    newSession :: AppM (UUID, SessionId)
    newSession = runDB $ do
        ticket <- generateUniqueTicket
        (ticket, ) <$> insert Session
            { sessionTicket  = UUIDField ticket
            , sessionType    = AccountSync
            , sessionStatus_ = Initiated
            , sessionError   = Nothing
            , sessionCompany = Nothing
            }
    isValidPassword Company { companyPassword } passwordAttempt =
        validatePassword (encodeUtf8 companyPassword)
                         (encodeUtf8 passwordAttempt)

-- | Signals authentication failure during QuickBooks syncing.
data AuthFailure
    = AuthFailure
    deriving (Show, Read, Eq)
instance Exception AuthFailure

-- | Signal an authentication failure.
authFailure :: MonadCatch m => m a
authFailure = throw AuthFailure

-- | Update the session status & error and return a failure authenticaton
-- result.
handleAuthFailure :: SessionId -> AuthFailure -> AppSqlM AuthResult
handleAuthFailure sessionId AuthFailure = do
    update
        sessionId
        [ SessionStatus_ =. Completed
        , SessionError =. Just InvalidAuthentication
        ]
    return InvalidUser


-- | Generate a 'Session' 'UUID' that does not yet exist in the
-- database.
generateUniqueTicket :: AppSqlM UUID
generateUniqueTicket = do
    ticket <- liftIO randomIO
    getBy (UniqueTicket $ UUIDField ticket) >>= \case
        Nothing -> return ticket
        Just _  -> generateUniqueTicket
