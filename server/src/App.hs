{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module App
    ( app
    , AppEnv(..)
    )
where

import           Api                            ( API
                                                , api
                                                )
import           Config                         ( AppConfig(..) )
import           Control.Exception.Safe         ( MonadThrow
                                                , try
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.IO.Unlift        ( MonadUnliftIO(..)
                                                , wrappedWithRunInIO
                                                )
import           Control.Monad.Except           ( ExceptT(..) )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , runReaderT
                                                , asks
                                                )
import           Data.Pool                      ( Pool )
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text                     as T
import           Data.UUID                      ( UUID )
import           Data.Version                   ( showVersion )
import           Database.Persist.Sql           ( SqlBackend
                                                , runSqlPool
                                                , insert
                                                , getBy
                                                )
import           DB.Schema                      ( Session(..)
                                                , Unique(UniqueTicket)
                                                )
import           DB.Fields                      ( UUIDField(..)
                                                , SessionType(..)
                                                , SessionStatus(..)
                                                , SessionError(..)
                                                )
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
import           Servant.API
import           Servant.Server                 ( ServerT
                                                , Server
                                                , Handler(..)
                                                , serve
                                                , hoistServer
                                                )
import           System.Random                  ( randomIO )

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

-- | The environment the application runs in. This includes static
-- configuration data as well as global runtime resources like the database
-- connection pool.
data AppEnv
    = AppEnv
        { appConfig :: AppConfig
        , appDBPool :: Pool SqlBackend
        }

-- | The monadic stack the handler routes run under.
newtype AppM a
    = AppM { fromAppM :: ReaderT AppEnv IO a }
    deriving (Functor, Applicative, Monad, MonadThrow, MonadIO, MonadReader AppEnv)

-- | Wrap/unwrap the ReaderT instance with 'AppM'/'fromAppM' calls.
instance MonadUnliftIO AppM where
    withRunInIO = wrappedWithRunInIO AppM fromAppM

-- | The monad for the application's database queries.
type AppSqlM a = ReaderT SqlBackend AppM a

-- | Run a series of database queries in a transaction. The transaction
-- will be rolled back when an exception is thrown.
runDB :: AppSqlM a -> AppM a
runDB query = asks appDBPool >>= runSqlPool query


-- | Join the separate route handlers to create our API.
server :: ServerT API AppM
server = generateAccountSyncQwc :<|> certRoute :<|> accountQuery


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
generateAccountSyncQwc :: MonadReader AppEnv m => m (QWCConfig, Text)
generateAccountSyncQwc = do
    cfg <- asks appConfig
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
        -- TODO: pass companyfile if known
        -- TODO: Use bcrypt to hash password & compare to stored hash
        expectedUser <- asks $ appAccountSyncUsername . appConfig
        expectedPass <- asks $ appAccountSyncPassword . appConfig
        let (authResult, status, authError) =
                if expectedUser /= user || expectedPass /= pass
                    then (InvalidUser, Completed, Just InvalidAuthentication)
                    else (ValidUser, Initiated, Nothing)
        ticket <- runDB $ do
            ticket <- generateUniqueTicket
            insert Session
                { sessionTicket = UUIDField ticket
                , sessionType   = AccountSync
                , sessionStatus = status
                , sessionError  = authError
                }
            return ticket
        return $ AuthenticateResp ticket authResult Nothing Nothing

-- | Generate a 'Session' 'UUID' that does not yet exist in the
-- database.
generateUniqueTicket :: AppSqlM UUID
generateUniqueTicket = do
    ticket <- liftIO randomIO
    getBy (UniqueTicket $ UUIDField ticket) >>= \case
        Nothing -> return ticket
        Just _  -> generateUniqueTicket
