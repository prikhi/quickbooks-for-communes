{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{- | This module defines the Database schema for the appliation, as well as
a migration function for creating/updating the tables.

-}
module DB.Schema
    (
    -- * Migrations
      migrateAll
    -- * Database Types
    -- ** QuickBooks Sessions
    , Session(..)
    , SessionId
    -- ** Accounts
    , Account(..)
    , AccountId
    -- ** Companies
    , Company(..)
    , CompanyId
    -- ** Entries
    -- *** Trips
    , StoreAccount(..)
    , StoreAccountId
    , Trip(..)
    , TripId
    , TripStop(..)
    , TripStopId
    , TripTransaction(..)
    , TripTransactionId
    , StoreCreditTransaction(..)
    , StoreCreditTransactionId
    -- ** Misc
    , Unique(..)
    , EntityField(..)
    )
where

import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Database.Persist.Sql
import           Database.Persist.TH
import           DB.Fields                      ( UUIDField
                                                , SessionError
                                                , SessionStatus
                                                , SessionType
                                                , AccountTypeField
                                                , EntryStatus
                                                )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Session
    ticket UUIDField
    type SessionType
    status_ SessionStatus sql=status
    company CompanyId Maybe
    error SessionError Maybe
    UniqueTicket ticket
    deriving Show Read



Company
    name Text
    user Text
    password Text
    fileName Text Maybe
    lastSyncTime UTCTime Maybe
    UniqueCompanyName name
    UniqueCompanyUser user
    deriving Show Read

Account
    name Text
    listId Text
    type AccountTypeField
    parent AccountId Maybe
    description Text
    isActive Bool
    company CompanyId
    modifiedTime UTCTime
    UniqueListId company listId
    deriving Show Read



StoreAccount
    name Text
    account AccountId
    deriving Show Read

Trip
    date UTCTime
    author Text
    number Text
    cashAdvance Rational
    cashReturned Rational
    status EntryStatus
    comment Text
    company CompanyId
    deriving Show Read

TripStop
    trip TripId
    name Text
    deriving Show Read

TripTransaction
    stop TripStopId
    account AccountId
    memo Text
    amount Rational
    tax Rational
    total Rational
    isReturn Bool
    deriving Show Read

StoreCreditTransaction
    trip TripId
    store StoreAccountId
    account AccountId
    memo Text
    amount Rational
    tax Rational
    total Rational
    isReturn Bool
    deriving Show Read
|]
