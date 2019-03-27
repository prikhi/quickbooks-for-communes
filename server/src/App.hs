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
                                                , when
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
import qualified Data.HashMap.Strict           as HM
import qualified Data.List                     as L
import           Data.Maybe                     ( fromMaybe
                                                , isNothing
                                                )
import           Data.Text                      ( pack )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8
                                                , decodeUtf8
                                                )
import           Data.Time                      ( UTCTime
                                                , TimeZone(timeZoneSummerOnly)
                                                , getCurrentTime
                                                , getTimeZone
                                                , addUTCTime
                                                )
import           Data.UUID                      ( UUID )
import           Data.Version                   ( showVersion )
import           Database.Persist.Sql           ( (=.)
                                                , Entity(..)
                                                , insert
                                                , insert_
                                                , insertBy
                                                , update
                                                , get
                                                , getBy
                                                )
import           DB.Schema                      ( Session(..)
                                                , SessionId
                                                , Account(..)
                                                , AccountId
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
import           QuickBooks.QBXML               ( Request(AccountQuery)
                                                , AccountQueryFilters(..)
                                                , Response(AccountQueryResponse)
                                                , AccountData(..)
                                                , ListReference(..)
                                                , OptionalListReference(..)
                                                )
import           QuickBooks.WebConnector        ( QWCConfig(..)
                                                , QBType(..)
                                                , Schedule(..)
                                                , Callback(..)
                                                , CallbackResponse(..)
                                                , AuthResult(..)
                                                , Username(..)
                                                , Password(..)
                                                , GetLastErrorResult(LastError)
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
companyQwcConfig :: AppConfig -> Company -> QWCFile
companyQwcConfig cfg c = QWCFile
    ( QWCConfig
        { qcAppDescription     = "Syncing Accounts to " <> appName cfg
        , qcAppDisplayName     = Nothing
        , qcAppID              = "QBFC_AS"
        , qcAppName            = appName cfg <> " - " <> companyName c
        , qcAppSupport         = url "/support/"
        , qcAppUniqueName      = Nothing
        , qcAppURL             = url $ appBaseUrl cfg <> "/accountSync/"
        , qcCertURL            = Just $ url $ appBaseUrl cfg <> "/cert/"
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
        [ "https://"
        , appHostname cfg
        , if appPort cfg /= 80 then ":" <> pack (show $ appPort cfg) else ""
        , path
        ]


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
    InitialSendRequestXML ticket _ _ _ filename _ _ _ ->
        runDB $ validateTicket ticket >>= \case
            Nothing -> return $ SendRequestXMLResp $ Left ()
            Just (Entity sessionId _, Entity companyId company) -> do
                when (noStoredFileName company)
                    $ update companyId [CompanyFileName =. Just filename]
                update sessionId [SessionStatus_ =. RequestedAccounts]
                lastSyncTime <- maybe (return Nothing) adjustSyncTime
                    $ companyLastSyncTime company
                return
                    $   SendRequestXMLResp
                    $   Right
                    $   AccountQuery
                    $   AccountQueryFilters
                    <$> lastSyncTime
    ReceiveResponseXML ticket resp -> runDB (validateTicket ticket) >>= \case
        Just (Entity sessionId _, Entity companyId _) -> case resp of
            AccountQueryResponse accounts -> do
                runDB $ update sessionId [SessionStatus_ =. UpdatingAccounts]
                runDB $ do
                    updateAccounts companyId accounts
                    now <- liftIO getCurrentTime
                    update companyId [CompanyLastSyncTime =. Just now]
                return $ ReceiveResponseXMLResp 100
            _ -> return $ ReceiveResponseXMLResp (-1)
        Nothing -> return $ ReceiveResponseXMLResp (-2)
    CloseConnection ticket -> runDB $ getSession ticket >>= \case
        Nothing -> return $ CloseConnectionResp "Unable to identify session."
        Just (Entity sessionId session) -> do
            update sessionId [SessionStatus_ =. Completed]
            case sessionError session of
                Nothing ->
                    return $ CloseConnectionResp "Sync Completed Successfully"
                Just err ->
                    return $ CloseConnectionResp $ "Ran Into Error: " <> pack
                        (show err)
    ConnectionError ticket _ message -> runDB $ validateTicket ticket >>= \case
        Nothing -> return $ ConnectionErrorResp "done"
        Just (Entity sessionId session, Entity _ company) -> do
            attemptNumber <- case sessionStatus_ session of
                HandlingConnectionError i ->
                    update
                            sessionId
                            [SessionStatus_ =. HandlingConnectionError (i + 1)]
                        >> return (i + 1)
                _ ->
                    update sessionId
                           [SessionStatus_ =. HandlingConnectionError 1]
                        >> return 1
            if attemptNumber > 2
                then abortConnectionRetrying sessionId message
                else
                    maybe (abortConnectionRetrying sessionId message)
                          (return . ConnectionErrorResp)
                        $ companyFileName company
    GetLastError ticket ->
        runDB
            $   getSession ticket
            >>= fmap (GetLastErrorResp . LastError)
            .   \case
                    Nothing -> return "Unable to identify session."
                    Just (Entity sessionId session) -> do
                        let
                            message = case sessionError session of
                                Just err -> "Encountered error while syncing: "
                                    <> pack (show err)
                                Nothing ->
                                    "Session called getLastError, but "
                                        <> "session has no error set!"
                        update sessionId [SessionStatus_ =. ErrorReported message]
                        return message
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

    isValidPassword :: Company -> T.Text -> Bool
    isValidPassword Company { companyPassword } passwordAttempt =
        validatePassword (encodeUtf8 companyPassword)
                         (encodeUtf8 passwordAttempt)

    getSession :: UUID -> AppSqlM (Maybe (Entity Session))
    getSession = getBy . UniqueTicket . UUIDField

    validateTicket :: UUID -> AppSqlM (Maybe (Entity Session, Entity Company))
    validateTicket ticket = getSession ticket >>= \case
        Nothing                   -> return Nothing
        Just s@(Entity _ session) -> case sessionCompany session of
            Nothing -> return Nothing
            Just companyId ->
                fmap (\c -> (s, Entity companyId c)) <$> get companyId

    noStoredFileName :: Company -> Bool
    noStoredFileName = maybe True T.null . companyFileName

    -- Stop retrying the quickbooks connection in the ConnectionError
    -- callback.
    abortConnectionRetrying :: SessionId -> T.Text -> AppSqlM CallbackResponse
    abortConnectionRetrying sessionId message =
        update
                sessionId
                [ SessionError =. Just (QuickBooksConnectionError message)
                , SessionStatus_ =. Completed
                ]
            >> return (ConnectionErrorResp "done")

    -- | Quickbook's doesn't use DST so our sync times during the summer
    -- will be an hour behind Quickbook's modified times, causing
    -- Quickbooks to return extraneous accounts. This function bumps up the
    -- sync time by an hour if necessary.
    adjustSyncTime :: MonadIO m => UTCTime -> m (Maybe UTCTime)
    adjustSyncTime syncTime = do
        isSummer <- liftIO $ timeZoneSummerOnly <$> getTimeZone syncTime
        return . Just $ if isSummer
            then fromInteger (60 * 60) `addUTCTime` syncTime
            else syncTime




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


updateAccounts :: CompanyId -> [AccountData] -> AppSqlM ()
updateAccounts companyId accounts = do
    let (roots, dataMap, childMap) = foldl
            (\(roots_, dataMap_, childMap_) account ->
                ( buildRootList roots_ account
                , buildDataMap dataMap_ account
                , buildChildMap childMap_ account
                )
            )
            ([], HM.empty, HM.empty)
            accounts
    mapM_ (runUpdate dataMap childMap Nothing) roots
  where
    buildRootList :: [ListReference] -> AccountData -> [ListReference]
    buildRootList roots AccountData { parentAccount, accountReference } =
        if isBlank parentAccount then accountReference : roots else roots

    buildDataMap
        :: HM.HashMap ListReference AccountData
        -> AccountData
        -> HM.HashMap ListReference AccountData
    buildDataMap acc a@AccountData { accountReference } =
        HM.insert accountReference a acc

    buildChildMap
        :: HM.HashMap ListReference [ListReference]
        -> AccountData
        -> HM.HashMap ListReference [ListReference]
    buildChildMap acc AccountData { parentAccount, accountReference } =
        case parentAccount of
            Just OptionalListReference { optionalListID, optionalFullName } ->
                case (optionalListID, optionalFullName) of
                    (Just listID, Just fullName) ->
                        let parentReference =
                                ListReference {listID , fullName }
                        in  HM.insertWith (\new old -> L.nub $ new <> old)
                                          parentReference
                                          [accountReference]
                                          acc
                    _ -> acc
            _ -> acc

    isBlank :: Maybe OptionalListReference -> Bool
    isBlank =
        maybe True
            $ \OptionalListReference { optionalListID, optionalFullName } ->
                  case (optionalListID, optionalFullName) of
                      (Nothing, Nothing) -> True
                      _                  -> False

    runUpdate
        :: HM.HashMap ListReference AccountData
        -> HM.HashMap ListReference [ListReference]
        -> Maybe AccountId
        -> ListReference
        -> AppSqlM ()
    runUpdate dataMap childMap maybeParent accountRef =
        let children = fromMaybe [] $ HM.lookup accountRef childMap
        in
            case HM.lookup accountRef dataMap of
                Nothing -> return ()
                Just accountData ->
                    let account = transform maybeParent accountData
                    in
                        do
                            accountId <- either entityKey id
                                <$> insertBy account
                            mapM_
                                (runUpdate dataMap childMap $ Just accountId)
                                children

    transform :: Maybe AccountId -> AccountData -> Account
    transform accountParent AccountData {..} = Account
        { accountName
        , accountListId       = listID accountReference
        , accountType
        , accountParent
        , accountIsActive     = isActive
        , accountModifiedTime = modifiedAt
        , accountCompany      = companyId
        }
