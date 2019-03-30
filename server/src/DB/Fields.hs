{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{- | Defines database field types & their Persistent instances. -}
module DB.Fields
    ( -- * Session Fields
      SessionType(..)
    , SessionStatus(..)
    , SessionError(..)
      -- * Account Fields
    , AccountType(..)
      -- * Entry Fields
    , EntryStatus(..)
      -- * Generic Fields
    , UUIDField(..)
    )
where

import           Data.UUID                      ( UUID )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Database.Persist.Sql
import           Database.Persist.TH            ( derivePersistField )
import           QuickBooks.QBXML               ( AccountType(..) )


-- SESSIONS

-- | Is the WebConnector 'DB.Schema.Session' for syncing accounts or
-- importing approved entries?
data SessionType
    = AccountSync
    deriving (Show, Read, Eq)

$(derivePersistField "SessionType")

-- | The status of a WebConnector 'DB.Schema.Session'.
data SessionStatus
    = Initiated
    | Authenticated
    | RequestedAccounts
    | UpdatingAccounts
    | HandlingConnectionError Integer
    | ErrorReported Text
    | Completed
    deriving (Show, Read, Eq)

$(derivePersistField "SessionStatus")

-- | Possible errors during WebConnector 'DB.Schema.Session' processing.
data SessionError
    = InvalidAuthentication
    | QuickBooksConnectionError Text
    deriving (Show, Read, Eq)

$(derivePersistField "SessionError")


-- ACCOUNTS

$(derivePersistField "AccountType")


-- ENTRIES

-- | What stage is an entry in?
data EntryStatus
    = Unapproved
    -- ^ Submitted by a Communard but not approved by an Accountant
    | Approved
    -- ^ Approved by an Accountant but not exported to QuickBooks
    | Exported
    -- ^ The entry has been exported to QuickBooks.
    deriving (Read, Show, Eq)

$(derivePersistField "EntryStatus")


-- GENERIC

-- | A newtype wrapper for UUIDs so we can add Persistent instances.
newtype UUIDField = UUIDField { unUUIDField :: UUID }

-- | Re-use the 'UUID' 'Show' instance.
instance Show UUIDField where
    show = show . unUUIDField

-- | Re-use the 'UUID' 'Read' instance.
instance Read UUIDField where
    readsPrec precedence str =
        (\(uuid, rest) -> (UUIDField uuid, rest))
            <$> readsPrec precedence str

-- | Store UUIDs as their raw 'Data.Text.Text' values.
instance PersistField UUIDField where
    toPersistValue = PersistText . pack . show
    fromPersistValue v =
        case fromPersistValue v of
            Left e ->
                Left e
            Right s ->
                case reads $ unpack s of
                    (x, _):_ ->
                        Right $ UUIDField x
                    [] ->
                        Left $ "Invalid UUIDField: " <> s

-- | Store UUIDs as a 'SqlString'
instance PersistFieldSql UUIDField where
    sqlType _ = SqlString
