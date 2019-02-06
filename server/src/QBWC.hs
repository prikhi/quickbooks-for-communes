{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module QBWC
    ( -- * QWC File Generation
      QWCConfig(..)
    , AuthFlag(..)
    , PersonalDataPreference(..)
    , QBType(..)
    , Schedule(..)
    , SOAPStyle(..)
    , UnattendedModePreference(..)
    , generateConnectorFile
    )
where

import           Data.ByteString                ( ByteString )
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.UUID                      ( UUID )
import qualified Data.UUID                     as UUID
import           Text.XML.Generator

data QWCConfig = QWCConfig
    { qcAppDescription :: Text
    , qcAppDisplayName :: Maybe Text
    , qcAppID :: Text
    , qcAppName :: Text
    , qcAppSupport :: Text
    , qcAppUniqueName :: Maybe Text
    , qcAppURL :: Text
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
    :: QWCConfig -- ^ The Application's Web Connector Configuration
    -> Text -- ^ The username
    -> ByteString -- ^ The render XML of the QWC File.
generateConnectorFile QWCConfig {..} userName =
    xrender $ doc defaultDocInfo $ xelem "QBWCXML" $ xelems $ catMaybes
        [ justXText "AppDescription" qcAppDescription
        , maybeElem "AppDisplayName" qcAppDisplayName
        , justXText "AppID"      qcAppID
        , justXText "AppName"    qcAppName
        , justXText "AppSupport" qcAppSupport
        , maybeElem "AppUniqueName" qcAppUniqueName
        , justXText "AppURL" qcAppURL
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
