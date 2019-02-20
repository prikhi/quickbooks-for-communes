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
    )
where

import           Control.Applicative            ( (<|>) )
import           Control.Monad.Catch.Pure       ( MonadThrow(..)
                                                , SomeException
                                                )
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.UUID                      ( UUID )
import qualified Data.UUID                     as UUID
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
import           Text.XML                       ( Node(..)
                                                , Element(..)
                                                , Name
                                                )
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
    deriving (Show)

instance FromXML Callback where
    fromXML e = parseServerVersion e
            <|> parseClientVersion e
            <|> parseAuthenticate e
            <|> parsingError "Unsupported Callback"

parseServerVersion :: MonadThrow m => Element -> m Callback
parseServerVersion el =
    if elementName el == "{http://developer.intuit.com/}serverVersion"
        then return ServerVersion
        else parsingError "ServerVersion parse failure"

parseClientVersion :: MonadThrow m => Element -> m Callback
parseClientVersion el =
    if elementName el == "{http://developer.intuit.com/}clientVersion"
        then case elementNodes el of
            [NodeElement strVersion] ->
                ClientVersion
                    <$> parseTextElement
                            "{http://developer.intuit.com/}strVersion"
                            strVersion
            e -> parsingError $ "Invalid clientVersion Body" ++ show e
        else parsingError "ClientVersion parse failure"

parseAuthenticate :: MonadThrow m => Element -> m Callback
parseAuthenticate el =
    if elementName el == "{http://developer.intuit.com/}authenticate"
        then case elementNodes el of
            [NodeElement userEl, NodeElement passEl] -> do
                user <-
                    Username
                        <$> parseTextElement
                                "{http://developer.intuit.com/}strUserName"
                                userEl
                pass <-
                    Password
                        <$> parseTextElement
                                "{http://developer.intuit.com/}strPassword"
                                passEl
                return $ Authenticate user pass
            _ -> parsingError "Invalid authenticate Content"
        else parsingError "Invalid authenticate Body"

-- | Parse a specific XML element containing only Text content.
parseTextElement :: MonadThrow m => Name -> Element -> m Text
parseTextElement name tEl = if elementName tEl == name
    then case elementNodes tEl of
        [NodeContent v] -> return v
        _ -> parsingError $ "Invalid Text Element Content for " <> show name
    else parsingError $ "Invalid Text Element Body for " <> show name

-- | Throw an XML parsing error.
parsingError :: MonadThrow m => String -> m a
parsingError s = throwM (error s :: SomeException)


data CallbackResponse
    = ServerVersionResp Text
    -- ^ Respond with the Server's version number.
    | ClientVersionResp Text
    -- ^ Accept the Client's version or issue a warning or error.
    -- TODO: Refactor Text into Type w/ valid states(Accept, Warn, Error)
    | AuthenticateResp UUID AuthResult (Maybe PostponeMinutes) (Maybe AutorunMinutes)
    -- ^ Return a session ticket & the authentication result
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
