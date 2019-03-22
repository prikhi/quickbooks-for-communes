{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types
    ( AppEnv(..)
    , AppM(..)
    , AppSqlM
    , runDB
    )
where

import           Config                         ( AppConfig(..) )
import           Control.Exception.Safe         ( MonadThrow )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.IO.Unlift        ( MonadUnliftIO(..)
                                                , wrappedWithRunInIO
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , asks
                                                )
import           Data.Pool                      ( Pool )
import           Database.Persist.Sql           ( SqlBackend
                                                , runSqlPool
                                                )

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
    deriving (Functor, Applicative, Monad, MonadThrow, MonadIO, MonadReader AppEnv)

-- | Wrap/unwrap the ReaderT instance with 'AppM'/'fromAppM' calls.
instance MonadUnliftIO AppM where
    withRunInIO = wrappedWithRunInIO AppM fromAppM

-- | The monad for the application's database queries.
type AppSqlM a = ReaderT SqlBackend AppM a

-- | Run a series of database queries in a transaction. The transaction
-- will be rolled back when an exception is thrown.
runDB :: AppSqlM a -> AppM a
runDB query = asks appDBPool >>= runSqlPool query
