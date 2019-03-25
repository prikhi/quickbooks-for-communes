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
    , Session(..)
    , SessionId
    , Company(..)
    , CompanyId
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
|]
