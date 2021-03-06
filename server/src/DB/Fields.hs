{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{- | Defines database field types & their Persistent instances. -}
module DB.Fields
    ( -- * Session Fields
      SessionType(..)
    , SessionStatus(..)
    , SessionError(..)
      -- * Account Fields
    , AccountTypeField(..)
      -- * Entry Fields
    , EntryStatus(..)
    , Cents(..)
    , Percentage(..)
      -- * Generic Fields
    , UUIDField(..)
    )
where

import           Data.Aeson                     ( (.=)
                                                , (.:)
                                                , ToJSON(..)
                                                , FromJSON(..)
                                                , Value(String)
                                                , withObject
                                                , object
                                                )
import           Data.Proxy                     ( Proxy(..) )
import           Data.UUID                      ( UUID )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Database.Persist.Sql
import           Database.Persist.TH            ( derivePersistField )
import           GHC.Generics                   ( Generic )
import           GHC.Natural                    ( Natural )
import           QuickBooks.QBXML               ( AccountType(..) )
import           Text.Read                      ( Read(readPrec) )


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

-- | A newtype wrapper for AccountTypes so we can add Persistent instances.
newtype AccountTypeField
    = AccountTypeField { unAccountTypeField :: AccountType }
    deriving (Generic)

-- | Re-use the 'AccountType' 'Show' instance.
instance Show AccountTypeField where
    show = show . unAccountTypeField

-- | Re-use the 'AccountType' 'Read' instance.
instance Read AccountTypeField where
    readsPrec precedence str =
        (\(accType, rest) -> (AccountTypeField accType, rest))
            <$> readsPrec precedence str

instance ToJSON AccountTypeField where
    toJSON = String . pack . show

$(derivePersistField "AccountTypeField")


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


-- | Dollar values are represented in number of Cents.
newtype Cents
    = Cents
        { fromCents :: Natural }
    deriving (Ord, Eq, Num)

-- | Show using the `Natural` instance
instance Show Cents where
    show = show . fromCents

-- | Read as `Natural` values
instance Read Cents where
    readPrec = Cents <$> readPrec

instance PersistField Cents where
    toPersistValue (Cents c) = toPersistValue c
    fromPersistValue = fmap Cents <$> fromPersistValue

instance PersistFieldSql Cents where
    sqlType = const $ sqlType (Proxy :: Proxy Natural)

-- | @{ "cents": \<int-value\> }@
instance ToJSON Cents where
    toJSON (Cents cents) = object ["cents" .= toJSON cents]

instance FromJSON Cents where
    parseJSON = withObject "Cents" $ \v -> do
        natural <- v .: "cents"
        return $ Cents natural


-- | Percentage values are represented as hundredths of a Percentage.
newtype Percentage
    = Percentage
        { fromPercentage :: Natural }
    deriving (Show, Read, Ord, Eq, Num)

instance PersistField Percentage where
    toPersistValue (Percentage p) = toPersistValue p
    fromPersistValue = fmap Percentage <$> fromPersistValue

instance PersistFieldSql Percentage where
    sqlType = const $ sqlType (Proxy :: Proxy Natural)

-- | @{ "percentage": \<int-value\> }@
instance ToJSON Percentage where
    toJSON (Percentage p) = object ["percentage" .= toJSON p]

instance FromJSON Percentage where
    parseJSON =
        withObject "Percentage" $ \v -> Percentage <$> (v .: "percentage")




-- GENERIC

-- | A newtype wrapper for UUIDs so we can add Persistent instances.
newtype UUIDField = UUIDField { unUUIDField :: UUID }

-- | Re-use the 'UUID' 'Show' instance.
instance Show UUIDField where
    show = show . unUUIDField

-- | Re-use the 'UUID' 'Read' instance.
instance Read UUIDField where
    readsPrec precedence str =
        (\(uuid, rest) -> (UUIDField uuid, rest)) <$> readsPrec precedence str

-- | Store UUIDs as their raw 'Data.Text.Text' values.
instance PersistField UUIDField where
    toPersistValue = PersistText . pack . show
    fromPersistValue v = case fromPersistValue v of
        Left  e -> Left e
        Right s -> case reads $ unpack s of
            (x, _) : _ -> Right $ UUIDField x
            []         -> Left $ "Invalid UUIDField: " <> s

-- | Store UUIDs as a 'SqlString'
instance PersistFieldSql UUIDField where
    sqlType _ = SqlString
