{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{- | Routes for the QuickBooks WebConnector.
-}
module Routes.QuickBooks
    ( routes
    , certRoute
    , syncRoute
    )
where

import           Api                            ( QuickBooksAPI )
import           Control.Exception.Safe         ( Exception
                                                , MonadCatch
                                                , catch
                                                , throw
                                                )
import           Control.Monad                  ( unless
                                                , when
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Crypto.BCrypt                  ( validatePassword )
import qualified Data.HashMap.Strict           as HM
import qualified Data.List                     as L
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( pack )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
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
                                                , insertBy
                                                , update
                                                , replace
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
                                                , AccountTypeField(..)
                                                )
import           Paths_qbfc                     ( version )
import           QuickBooks.QBXML               ( Request(AccountQuery)
                                                , AccountQueryFilters(..)
                                                , Response(AccountQueryResponse)
                                                , AccountData(..)
                                                , ListReference(..)
                                                , OptionalListReference(..)
                                                )
import           QuickBooks.WebConnector        ( Callback(..)
                                                , CallbackResponse(..)
                                                , AuthResult(..)
                                                , Username(..)
                                                , Password(..)
                                                , GetLastErrorResult(LastError)
                                                )
import           Servant.API
import           Servant.Server                 ( ServerT )
import           System.Random                  ( randomIO )
import           Types                          ( AppM(..)
                                                , AppSqlM
                                                , runDB
                                                )


-- | The assembled handlers for the 'QuickBooksAPI' type.
routes :: ServerT QuickBooksAPI AppM
routes = certRoute :<|> syncRoute


-- | An empty route that does absolutely nothing. This is used for SSL
-- certificate verification by the WebConnector.
certRoute :: Monad m => m NoContent
certRoute = return NoContent


-- | Perform querying/syncing operations for the QuickBooks Accounts.
--
-- TODO: Do both accounting pulling & entry pushing here?
syncRoute :: Callback -> AppM CallbackResponse
syncRoute r = case r of
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


-- | Update a Company's Accounts using the AccountData returned from
-- QuickBooks. First we attempt to insert/update Accounts from root nodes
-- down to all their descendents.
--
-- During partial updates, we do not get all the required root nodes.
-- Instead of top-down updating, we iterate through the list of accounts,
-- first inserting parents that are furthur down the list or finding them
-- in the database. After finding & upserting the parent, we use it's
-- AccountId to upsert the child Account.
updateAccounts :: CompanyId -> [AccountData] -> AppSqlM ()
updateAccounts companyId accounts = do
    let (roots, dataMap, childMap, refList) = foldl
            (\(roots_, dataMap_, childMap_, refList_) account ->
                ( buildRootList roots_ account
                , buildDataMap dataMap_ account
                , buildChildMap childMap_ account
                , accountReference account : refList_
                )
            )
            ([], HM.empty, HM.empty, [])
            accounts
    insertedRefs <- concat <$> mapM (runUpdate dataMap childMap Nothing) roots
    let uninserted = refList L.\\ insertedRefs
    processFragments dataMap uninserted
  where
    -- | Build the list of root ListReferences.
    buildRootList :: [ListReference] -> AccountData -> [ListReference]
    buildRootList roots AccountData { parentAccount, accountReference } =
        if isBlank parentAccount then accountReference : roots else roots

    -- | Build a mapping from ListReferences to QuickBooks AccountData.
    buildDataMap
        :: HM.HashMap ListReference AccountData
        -> AccountData
        -> HM.HashMap ListReference AccountData
    buildDataMap acc a@AccountData { accountReference } =
        HM.insert accountReference a acc

    -- | Build a mapping from Parent ListReferences to Child
    -- ListReferences.
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

    -- | Are all fields in the OptionalListReference Nothing?
    isBlank :: Maybe OptionalListReference -> Bool
    isBlank =
        maybe True
            $ \OptionalListReference { optionalListID, optionalFullName } ->
                  case (optionalListID, optionalFullName) of
                      (Nothing, Nothing) -> True
                      _                  -> False

    -- | Insert/Update the account, given the data map, child map, & parent
    -- account ID.
    runUpdate
        :: HM.HashMap ListReference AccountData
        -> HM.HashMap ListReference [ListReference]
        -> Maybe AccountId
        -> ListReference
        -> AppSqlM [ListReference]
    runUpdate dataMap childMap maybeParent accountRef
        = let children = fromMaybe [] $ HM.lookup accountRef childMap
          in
              case HM.lookup accountRef dataMap of
                  Nothing          -> return [accountRef]
                  Just accountData -> do
                      let account = transform maybeParent accountData
                      accountId <- either entityKey id <$> insertBy account
                      childRefs <-
                          concat
                              <$> mapM
                                      ( runUpdate dataMap childMap
                                      $ Just accountId
                                      )
                                      children
                      return $ accountRef : childRefs

    -- | Build an Account from it's parent ID & the QuickBooks AccountData.
    transform :: Maybe AccountId -> AccountData -> Account
    transform accountParent AccountData {..} = Account
        { accountName
        , accountListId       = listID accountReference
        , accountType         = AccountTypeField accountType
        , accountParent
        , accountDescription  = fromMaybe "" description
        , accountIsActive     = isActive
        , accountModifiedTime = modifiedAt
        , accountCompany      = companyId
        }

    -- | Insert the account if the unique list reference doesn't exist, otherwise
    -- replace the existing account.
    uniqueRepsert :: Account -> AppSqlM AccountId
    uniqueRepsert account =
        getBy (UniqueListId companyId $ accountListId account) >>= \case
            Nothing -> insert account
            Just (Entity accountId _) ->
                replace accountId account >> return accountId

    -- | Insert/Update Accounts whose roots were not in the account list.
    processFragments
        :: HM.HashMap ListReference AccountData -> [ListReference] -> AppSqlM ()
    processFragments dataMap refs = case refs of
        []              -> return ()
        ref : toProcess -> do
            (_, remaining) <- processFragment dataMap ref toProcess
            processFragments dataMap remaining

    -- | Insert/Update an Account whose root we were not given. First we
    -- attempt to find the parent AccountData in the list of references. If
    -- it exists, we process the parent first & use the returned ID to
    -- insert the child. If it does not exist, we get the parent's account
    -- ID from the database, using the parent's listID. After inserting the
    -- parent & child, we return the child's account ID & the remaining
    -- references that have no yet been updated.
    processFragment
        :: HM.HashMap ListReference AccountData
        -> ListReference
        -> [ListReference]
        -> AppSqlM (Maybe AccountId, [ListReference])
    processFragment dataMap ref remaining = case HM.lookup ref dataMap of
        Nothing      -> return (Nothing, remaining)
        Just account -> case getFullParentReference account of
            Nothing            -> return (Nothing, remaining)
            Just parentListRef -> case L.find (== parentListRef) remaining of
                Just parentRef -> do
                    (parentAccountId, remaining_) <-
                        processFragment dataMap parentRef
                            $ L.delete parentRef remaining
                    accountId <- uniqueRepsert
                        $ transform parentAccountId account
                    return (Just accountId, remaining_)
                Nothing ->
                    getBy (UniqueListId companyId $ listID parentListRef)
                        >>= \case
                                Just (Entity parentAccountId _) -> do
                                    accountId <- uniqueRepsert $ transform
                                        (Just parentAccountId)
                                        account
                                    return (Just accountId, remaining)
                                Nothing -> return (Nothing, remaining)

    -- | Build a ListReference for the Account's Parent.
    getFullParentReference :: AccountData -> Maybe ListReference
    getFullParentReference account = case parentAccount account of
        Nothing     -> Nothing
        Just optRef -> case (optionalListID optRef, optionalFullName optRef) of
            (Just listID, Just fullName) -> Just ListReference {..}
            _                            -> Nothing
