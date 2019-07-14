{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- | This module contains the environment the application runs with,
a monad that all routes execute in, and functionality-based typeclasses
that are used to limit the effects possible within discrete
routes/handlers/functions, instead of allowing access to the full 'AppM'
context.

-}
module Types
    ( -- * Application Types
      AppEnv(..)
    , AppM(..)
      -- * Typeclasses
      -- ** SQL
    , HasDBPool(..)
    , SqlM
    , SqlDB(..)
      -- ** Generation
    , GenerateUUID(..)
      -- ** Time
    , TimeAndZone(..)
      -- ** Hashing
    , HashPassword(..)
    )
where

import           Config                         ( AppConfig(..) )
import           Control.Exception.Safe         ( MonadThrow
                                                , MonadCatch
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.IO.Unlift        ( MonadUnliftIO(..)
                                                , wrappedWithRunInIO
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , asks
                                                )
import           Crypto.BCrypt                  ( hashPasswordUsingPolicy
                                                , slowerBcryptHashingPolicy
                                                )
import           Data.Pool                      ( Pool )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Data.Time                      ( UTCTime
                                                , TimeZone
                                                , getCurrentTime
                                                , getTimeZone
                                                )
import           Data.UUID                      ( UUID )
import           Database.Persist.Sql           ( SqlBackend
                                                , runSqlPool
                                                )
import           System.Random                  ( randomIO )

-- | The environment the application runs in. This includes static
-- configuration data as well as global runtime resources like the database
-- connection pool.
data AppEnv
    = AppEnv
        { appConfig :: AppConfig
        , appDBPool :: Pool SqlBackend
        }

-- | The monadic stack the handler routes run under.
newtype AppM a
    = AppM { fromAppM :: ReaderT AppEnv IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadThrow
             , MonadCatch
             , MonadIO
             , MonadReader AppEnv
             , GenerateUUID
             , SqlDB
             , HashPassword
             )

-- | Wrap/unwrap the ReaderT instance with 'AppM'/'fromAppM' calls.
instance MonadUnliftIO AppM where
    withRunInIO = wrappedWithRunInIO AppM fromAppM



-- Typeclasses

-- | The type has an SQL Pool Available.
class HasDBPool env where
    getDBPool :: env -> Pool SqlBackend

instance HasDBPool AppEnv where
    getDBPool = appDBPool


-- | The monad for the application's database queries.
type SqlM m a = ReaderT SqlBackend m a

-- | Can run database queries.
class MonadIO m => SqlDB m where
    runDB :: SqlM m a -> m a

instance (MonadUnliftIO m, HasDBPool env) => SqlDB (ReaderT env m) where
    runDB query = asks getDBPool >>= runSqlPool query


-- | Can generate new UUIDs.
class Monad m => GenerateUUID m where
    generateUUID :: m UUID

instance MonadIO m => GenerateUUID (ReaderT env m) where
    generateUUID = liftIO randomIO


-- | Get the current time or a time's timezone.
class Monad m => TimeAndZone m where
    -- | Get the current time.
    getTime :: m UTCTime
    -- | Get the timezone of time.
    getZone :: UTCTime -> m TimeZone

instance MonadIO m => TimeAndZone (ReaderT env m) where
    getTime = liftIO getCurrentTime
    getZone = liftIO . getTimeZone


-- | Hash a password for storage in the database.
class Monad m => HashPassword m where
    hashPassword :: Text -> m (Maybe Text)

instance MonadIO m => HashPassword (ReaderT env m) where
    hashPassword password =
        fmap decodeUtf8
            <$> liftIO
                    ( hashPasswordUsingPolicy slowerBcryptHashingPolicy
                    $ encodeUtf8 password
                    )
