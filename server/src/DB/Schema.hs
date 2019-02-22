{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{- | This module defines the Database schema for the appliation, as well as
a migration function for creating/updating the tables.

-}
module DB.Schema where

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
    status SessionStatus
    error SessionError Maybe
    UniqueTicket ticket
    deriving Show
|]
