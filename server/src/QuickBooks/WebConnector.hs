{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module QuickBooks.WebConnector
    ( -- * QWC File Generation
      QWCConfig(..)
    , AuthFlag(..)
    , PersonalDataPreference(..)
    , QBType(..)
    , Schedule(..)
    , SOAPStyle(..)
    , UnattendedModePreference(..)
    , generateConnectorFile
      -- * WebConnector Callbacks & Responses
    , Callback(..)
    , CallbackResponse(..)
    , Username(..)
    , Password(..)
    , AuthResult(..)
    , PostponeMinutes(..)
    , AutorunMinutes(..)
    , GetLastErrorResult(..)
    )
where

import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text                     as T
import           Data.UUID                      ( UUID )
import qualified Data.UUID                     as UUID
import           Parser                         ( Parser
                                                , parseError
                                                , matchName
                                                , oneOf
                                                , find
                                                , at
                                                , parseInteger
                                                , parseContent
                                                , parseContentWith
                                                , withNamespace
                                                )
import           QuickBooks.QBXML               ( Request(..)
                                                , buildRequest
                                                , Response(..)
                                                , HostData
                                                , CompanyData
                                                , PreferencesData
                                                )
import           Text.XML.Generator             ( Xml
                                                , Elem
                                                , Namespace
                                                , xelemWithText
                                                , xelem
                                                , xelems
                                                , xelemQ
                                                , xtext
                                                , namespace
                                                )
import           Text.XML                       ( Name )
import           XML                            ( FromXML(..)
                                                , ToXML(..)
                                                )

-- Config File

data QWCConfig = QWCConfig
    { qcAppDescription :: Text
    , qcAppDisplayName :: Maybe Text
    , qcAppID :: Text
    , qcAppName :: Text
    , qcAppSupport :: Text
    , qcAppUniqueName :: Maybe Text
    , qcAppURL :: Text
    , qcCertURL :: Maybe Text
    , qcAuthFlags :: [AuthFlag]
    , qcFileID :: UUID
    , qcIsReadOnly :: Bool
    , qcNotify :: Bool
    , qcOwnerID :: UUID
    , qcPersonalDataPref :: Maybe PersonalDataPreference
    , qcQBType :: QBType
    , qcScheduler :: Maybe Schedule
    , qcStyle :: Maybe SOAPStyle
    , qcUnattendedModePref :: Maybe UnattendedModePreference
    } deriving (Show)

-- | Generate the XML for a QWC File using a Configuration & Username.
instance ToXML (QWCConfig, Text) where
    toXML (c, u) = generateConnectorFile c u

data AuthFlag
    = AllEditions
    | QBSimpleStart
    | QBPro
    | QBPremier
    | QBEnterprise
    deriving (Show)

-- TODO: if list contains All, just return sum, else nub list and fold
andAuthFlags :: [AuthFlag] -> Integer
andAuthFlags = foldr (\flag acc -> toInt flag + acc) 0
  where
    toInt = \case
        AllEditions   -> 1 + 2 + 4 + 8
        QBSimpleStart -> 1
        QBPro         -> 2
        QBPremier     -> 4
        QBEnterprise  -> 8

data PersonalDataPreference
    = PersonalDataNotNeeded
    | PersonalDataOptional
    | PersonalDataRequired

instance Show PersonalDataPreference where
    show = \case
        PersonalDataNotNeeded -> "pdpNotNeeded"
        PersonalDataOptional  -> "pdpOptional"
        PersonalDataRequired  -> "pdpRequired"

data QBType
    = Financial
    | PointOfSale

instance Show QBType where
    show = \case
        Financial   -> "QBFS"
        PointOfSale -> "QBPOS"

data Schedule
    = EveryMinute Integer
    | EverySecond Integer
    deriving (Show)

xSchedule :: Schedule -> Xml Elem
xSchedule = \case
    EveryMinute i -> xelem "RunEveryNMinutes" . pack $ show i
    EverySecond i -> xelem "RunEveryNSeconds" . pack $ show i

data SOAPStyle
    = Document
    | DocWrapped
    | RPC
    deriving (Show)

data UnattendedModePreference
    = UnattendedModeOptional
    | UnattendedModeRequired

instance Show UnattendedModePreference where
    show = \case
        UnattendedModeOptional -> "umpOptional"
        UnattendedModeRequired -> "umpRequired"

-- | Generate the QWC XML configuration file for a QuickBooks User's Web
-- Connector.
generateConnectorFile
    :: QWCConfig -- ^ The Application's Web Connector Configuration.
    -> Text -- ^ The User's Login.
    -> Xml Elem -- ^ The XML representing the QWC File.
generateConnectorFile QWCConfig {..} userName =
    xelem "QBWCXML" $ xelems $ catMaybes
        [ justXText "AppDescription" qcAppDescription
        , maybeElem "AppDisplayName" qcAppDisplayName
        , justXText "AppID"      qcAppID
        , justXText "AppName"    qcAppName
        , justXText "AppSupport" qcAppSupport
        , maybeElem "AppUniqueName" qcAppUniqueName
        , justXText "AppURL" qcAppURL
        , maybeElem "CertURL" qcCertURL
        , justXText "AuthFlags" $ showT $ andAuthFlags qcAuthFlags
        , justXText "FileID" $ showUUID qcFileID
        , justXBool "IsReadOnly" qcIsReadOnly
        , justXBool "Notify"     qcNotify
        , justXText "OwnerID" $ showUUID qcOwnerID
        , maybeElemWith "PersonalDataPref" showT qcPersonalDataPref
        , justXText "QBType" $ showT qcQBType
        , fmap (xelem "Scheduler" . xSchedule) qcScheduler
        , maybeElemWith "Style"              showT qcStyle
        , maybeElemWith "UnattendedModePref" showT qcUnattendedModePref
        , justXText "UserName" userName
        ]
  where
    justXText name = Just . xelemWithText name
    maybeElem name = fmap $ xelemWithText name
    maybeElemWith name f = maybeElem name . fmap f
    justXBool name bool = justXText name $ if bool then "true" else "false"
    showUUID :: UUID -> Text
    showUUID uuid = "{" <> UUID.toText uuid <> "}"



-- Callbacks

data Callback
    = ServerVersion
    | ClientVersion Text
    | Authenticate Username Password
    -- | The first sendRequestXML callback received contains Host, Company,
    -- & Preference data.
    --
    -- TODO: Split out into separate record type so we can get named fields?
    | InitialSendRequestXML
        UUID                -- ^ The session ticket
        HostData            -- ^ HostQuery data for the running version of QuickBooks
        CompanyData         -- ^ CompanyQuery data for the company file
        PreferencesData     -- ^ PreferencesQuery data for the company file
        Text                -- ^ The Company Filename
        Text                -- ^ The Country version of QuickBooks
        Integer             -- ^ The Major version of the qbXML Processor
        Integer             -- ^ The Minor version of the qbXML Processor
    | ReceiveResponseXML
        UUID                -- ^ The session ticket
        Response            -- ^ The parsed qbXML response
    | CloseConnection
        UUID                -- ^ The session ticket
    | ConnectionError
        UUID                -- ^ The session ticket
        Text                -- ^ The @HRESULT@ hex-string thrown by the WebConnector.
        Text                -- ^ The error message for the HRESULT.
    | GetLastError
        UUID                -- ^ The session ticket
    deriving (Show)

instance FromXML Callback where
    fromXML = oneOf
        [ parseServerVersion
        , parseClientVersion
        , parseAuthenticate
        , parseInitialSendRequestXML
        , parseReceiveResponseXML
        , parseCloseConnection
        , parseConnectionError
        , parseGetLastError
        , parseError "Unsupported WebConnector Callback"
        ]


parseServerVersion :: Parser Callback
parseServerVersion = matchName (qbName "serverVersion") $ return ServerVersion

parseClientVersion :: Parser Callback
parseClientVersion =
    matchName (qbName "clientVersion")
        $   ClientVersion
        <$> find (qbName "strVersion") parseContent

parseAuthenticate :: Parser Callback
parseAuthenticate = matchName (qbName "authenticate") $ do
    user <- Username <$> find (qbName "strUserName") parseContent
    pass <- Password <$> find (qbName "strPassword") parseContent
    return $ Authenticate user pass

parseInitialSendRequestXML :: Parser Callback
parseInitialSendRequestXML = matchName (qbName "sendRequestXML") $ do
    (hostData, companyData, preferencesData) <-
        find (qbName "strHCPResponse")
        $   parseContentWith
        $   find "QBXMLMsgsRs"
        $   (,,)
        <$> at ["HostQueryRs", "HostRet"]               fromXML
        <*> at ["CompanyQueryRs", "CompanyRet"]         fromXML
        <*> at ["PreferencesQueryRs", "PreferencesRet"] fromXML
    ticket       <- find (qbName "ticket") parseUUID
    companyFile  <- find (qbName "strCompanyFileName") parseContent
    country      <- find (qbName "qbXMLCountry") parseContent
    majorVersion <- find (qbName "qbXMLMajorVers") parseInteger
    minorVersion <- find (qbName "qbXMLMinorVers") parseInteger
    return $ InitialSendRequestXML ticket
                                   hostData
                                   companyData
                                   preferencesData
                                   companyFile
                                   country
                                   majorVersion
                                   minorVersion

parseReceiveResponseXML :: Parser Callback
parseReceiveResponseXML = matchName (qbName "receiveResponseXML") $ do
    ticket <- find (qbName "ticket") parseUUID
    resp   <- find (qbName "response") $ parseContentWith $ find "QBXMLMsgsRs"
                                                                 fromXML
    return $ ReceiveResponseXML ticket resp

parseCloseConnection :: Parser Callback
parseCloseConnection =
    matchName (qbName "closeConnection")
        $   CloseConnection
        <$> find (qbName "ticket") parseUUID

parseConnectionError :: Parser Callback
parseConnectionError = matchName (qbName "connectionError") $ do
    ticket  <- find (qbName "ticket") parseUUID
    hresult <- find (qbName "hresult") parseContent
    message <- find (qbName "message") parseContent
    return $ ConnectionError ticket hresult message

parseGetLastError :: Parser Callback
parseGetLastError =
    matchName (qbName "getLastError")
        $   GetLastError
        <$> find (qbName "ticket") parseUUID

-- | Build an 'Element' name in the Intuit Developer 'Namespace'.
qbName :: Text -> Name
qbName = withNamespace "http://developer.intuit.com/"

-- | Parse a UUID that is enclosed in braces(@{}@).
parseUUID :: Parser UUID
parseUUID = do
    str <- parseContent
    case UUID.fromText (dropBrackets str) of
        Just val -> return val
        Nothing  -> parseError $ "Expected {UUID}, got: " <> str
    where dropBrackets = T.reverse . T.drop 1 . T.reverse . T.drop 1

-- | Valid responses for callbacks.
--
-- TODO: should we break these off into their own type so we can ensure the
-- ServerVersion callback returns a ServerVersionResp?
data CallbackResponse
    = ServerVersionResp Text
    -- ^ Respond with the Server's version number.
    | ClientVersionResp Text
    -- ^ Accept the Client's version or issue a warning or error.
    -- TODO: Refactor Text into Type w/ valid states(Accept, Warn, Error)
    | AuthenticateResp UUID AuthResult (Maybe PostponeMinutes) (Maybe AutorunMinutes)
    -- ^ Return a session ticket & the authentication result
    | SendRequestXMLResp (Either () Request)
    -- ^ Send the given qbXML request
    -- TODO: refactor: PauseOrError | MakeRequest Request
    | ReceiveResponseXMLResp Integer
    -- ^ Send the 0-100 for the percentage complete or a negative number to
    -- trigger a getLastError call.
    -- TODO: Refactor Integer into InProgress, Complete, or Error types.
    | CloseConnectionResp Text
    -- ^ Send the status text to show in the WebConnector UI.
    | ConnectionErrorResp Text
    -- ^ Send "done" to close the connection, or any other text as the path
    -- to a company file to open.
    -- TODO: Refactor Text -> Done | TryCompanyFile
    | GetLastErrorResp GetLastErrorResult
    -- ^ Return a message string describing the issue, switch into
    -- interactive mode, or pause for 5 seconds and call sendRequestXML
    -- again.
    deriving (Show)

instance ToXML CallbackResponse where
    toXML r = case r of
        ServerVersionResp v ->
            xelemQ qbNamespace "serverVersionResponse" $
                xelemQ qbNamespace "serverVersionResult" v
        ClientVersionResp v ->
            xelemQ qbNamespace "clientVersionResponse" $
                xelemQ qbNamespace "clientVersionResult" v
        AuthenticateResp ticket result maybePostpone maybeAutorun ->
            xelemQ qbNamespace "authenticateResponse" $
                xelemQ qbNamespace "authenticateResult" $
                    qbStringArray $ catMaybes
                        [ Just $ "{" <> UUID.toText ticket <> "}"
                        , Just $ showT result
                        , fmap showT maybePostpone
                        , fmap showT maybeAutorun
                        ]
        SendRequestXMLResp req ->
            xelemQ qbNamespace "sendRequestXMLResponse" $
                xelemQ qbNamespace "sendRequestXMLResult" $
                    xtext $ either (const "") buildRequest req
        ReceiveResponseXMLResp progress ->
            xelemQ qbNamespace "receiveResponseXMLResponse" $
                xelemQ qbNamespace "receiveResponseXMLResult" $
                    xtext $ showT progress
        CloseConnectionResp message ->
            xelemQ qbNamespace "closeConnectionResponse" $
                xelemQ qbNamespace "closeConnectionResult" $
                    xtext message
        ConnectionErrorResp message ->
            xelemQ qbNamespace "connectionErrorResponse" $
                xelemQ qbNamespace "connectionErrorResult" $
                    xtext message
        GetLastErrorResp result ->
            xelemQ qbNamespace "getLastErrorResponse" $
                xelemQ qbNamespace "getLastErrorResult" $
                    toXML result


-- | Render a Text list as a QuickBooks String Array.
qbStringArray :: [Text] -> Xml Elem
qbStringArray = xelems . map (xelemQ qbNamespace "string" . xtext)

-- | Render a showable type as Text.
showT :: Show a => a -> Text
showT = pack . show

-- | The XML Namespace for QuickBooks XML elements.
qbNamespace :: Namespace
qbNamespace = namespace "" "http://developer.intuit.com/"

-- Callback Types

-- | The Username passed to the 'Authenticate' callback.
newtype Username
    = Username { fromUsername :: Text }
    deriving (Show, Eq)

-- | The Password passed to the 'Authenticate' callback.
newtype Password
    = Password { fromPassword :: Text }
    deriving (Show, Eq)

-- | The result of an Authentication attempt.
data AuthResult
    = ValidUser -- ^ The given 'Username' & 'Password' is valid. Connect to
                -- the currently open QuickBooks Company File.
    | CompanyFile Text -- ^ The given 'Username' & 'Password' is valid.
                       -- Connect to the given QuickBooks Company File.
    | InvalidUser -- ^ An invalid 'Username' or 'Password' was supplied.
    | Busy -- ^ The Application is currently busy, try again later.
    deriving (Eq)

instance Show AuthResult where
    show val = case val of
        ValidUser -> ""
        InvalidUser -> "nvu"
        Busy -> "busy"
        CompanyFile t  -> unpack t

-- | The number of minutes to postpone an update by.
newtype PostponeMinutes
    = PostponeMinutes { fromPostponeMinutes :: Integer }
    deriving (Eq)

instance Show PostponeMinutes where
    show = show . fromPostponeMinutes

-- | The minimum interval between automatic updates.
newtype AutorunMinutes
    = AutorunMinutes { fromAutorunMinutes :: Integer }
    deriving (Eq)

instance Show AutorunMinutes where
    show = show . fromAutorunMinutes

-- | Possible response values for the getLastError callback.
data GetLastErrorResult
    = NoOp
    -- ^ Pause for 5 seconds and then call sendRequestXML again.
    | InteractiveMode
    -- ^ Switch to interactive mode and then call getInteractiveURL
    | LastError Text
    -- ^ Log & show the error in the WebConnector and close the connection.
    deriving (Show, Read, Eq)

instance ToXML GetLastErrorResult where
    toXML = xtext . \case
        NoOp ->
            "NoOp"
        InteractiveMode ->
            "Interactive mode"
        LastError message ->
            message
