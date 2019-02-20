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
    )
where

import           Control.Applicative            ( (<|>) )
import           Control.Monad.Catch.Pure       ( MonadThrow(..)
                                                , SomeException
                                                )
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text
                                                , pack
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
                                                , namespace
                                                )
import           Text.XML                       ( Node(..)
                                                , Element(..)
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
    showT :: Show a => a -> Text
    showT = pack . show
    showUUID :: UUID -> Text
    showUUID uuid = "{" <> UUID.toText uuid <> "}"


-- Callbacks

data Callback
    = ServerVersion
    | ClientVersion Text
    deriving (Show)

instance FromXML Callback where
    fromXML e = parseServerVersion e
            <|> parseClientVersion e

parseServerVersion :: MonadThrow m => Element -> m Callback
parseServerVersion el =
    if elementName el == "{http://developer.intuit.com/}serverVersion"
        then return ServerVersion
        else parsingError "ServerVersion parse failure"

parseClientVersion :: MonadThrow m => Element -> m Callback
parseClientVersion el =
    if elementName el == "{http://developer.intuit.com/}clientVersion"
        then case elementNodes el of
            [NodeElement strVersion] -> parseVersion strVersion
            e -> parsingError $ "Invalid clientVersion Body" ++ show e
        else parsingError "ClientVersion parse failure"
  where
    parseVersion vEl =
        if elementName vEl == "{http://developer.intuit.com/}strVersion"
            then case elementNodes vEl of
                [NodeContent v] -> return $ ClientVersion v
                _               -> parsingError "Invalid clientVersion Content"
            else parsingError $ "Invalid clientVersion Body" ++ show vEl

parsingError :: MonadThrow m => String -> m a
parsingError s = throwM (error s :: SomeException)

data CallbackResponse
    = ServerVersionResp Text
    | ClientVersionResp Text
    deriving (Show)

instance ToXML CallbackResponse where
    toXML r = case r of
        ServerVersionResp v ->
            xelemQ qbNamespace "serverVersionResponse" $
                xelemQ qbNamespace "serverVersionResult" v
        ClientVersionResp v ->
            xelemQ qbNamespace "clientVersionResponse" $
                xelemQ qbNamespace "clientVersionResult" v


qbNamespace :: Namespace
qbNamespace = namespace "" "http://developer.intuit.com/"
