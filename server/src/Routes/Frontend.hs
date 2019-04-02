{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- | API routes for the Frontend.
-}
module Routes.Frontend
    ( routes
    , newCompany
    , generateQwcFile
    )
where

import           Api                            ( FrontendAPI
                                                , QWCFile(..)
                                                , NewCompany(..)
                                                )
import           Config                         ( AppConfig(..) )
import           Control.Exception.Safe         ( MonadThrow
                                                , throw
                                                )
import           Control.Monad                  ( (>=>) )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                )
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( pack )
import qualified Data.Text                     as T
import           Database.Persist.Sql           ( insert_
                                                , get
                                                , getBy
                                                )
import           DB.Schema                      ( Company(..)
                                                , CompanyId
                                                , Unique(..)
                                                )
import           QuickBooks.WebConnector        ( QWCConfig(..)
                                                , QBType(..)
                                                , Schedule(..)
                                                )
import           Servant.API
import           Servant.Server                 ( ServerT
                                                , err404
                                                )
import           Types                          ( AppEnv(..)
                                                , AppM(..)
                                                , SqlDB(..)
                                                , HashPassword(..)
                                                )
import qualified Validation                    as V


-- | The assembled handlers for the 'FrontendAPI' type.
routes :: ServerT FrontendAPI AppM
routes = newCompany :<|> generateQwcFile


-- | Validate & create a new 'Company'.
--
-- TODO: Require user account via cookie auth
newCompany
    :: (MonadReader AppEnv m, HashPassword m, SqlDB m, MonadThrow m)
    => NewCompany
    -> m QWCFile
newCompany = V.validateOrThrow >=> \NewCompany {..} -> do
    hashedPassword <- hashPassword ncPassword
    cfg            <- asks appConfig
    case hashedPassword of
        Nothing -> V.validationError $ V.formError
            "There was an issue securing the password. Please try again."
        Just pass -> runDB $ do
            existingName <- nameError <$> getBy (UniqueCompanyName ncName)
            existingUser <- userError_ <$> getBy (UniqueCompanyUser ncUsername)
            let uniquenessTest = (,) <$> existingUser <*> existingName
            V.whenValid uniquenessTest $ \_ -> do
                let company :: Company = Company
                        { companyName         = ncName
                        , companyUser         = ncUsername
                        , companyPassword     = pass
                        , companyFileName     = Nothing
                        , companyLastSyncTime = Nothing
                        }
                insert_ company
                return $ companyQwcConfig cfg company
  where
    nameError =
        V.validate "name" "A company with this name already exists." isNothing
    userError_ = V.validate "username"
                            "A company is already using this username."
                            isNothing


-- Get a QWC File

-- | Generate a QWC File for a 'Company'.
generateQwcFile
    :: (MonadReader AppEnv m, SqlDB m, MonadThrow m) => CompanyId -> m QWCFile
generateQwcFile companyId = do
    cfg     <- asks appConfig
    company <- runDB $ get companyId >>= maybe (throw err404) return
    return $ companyQwcConfig cfg company

-- | Build the WebConnetor configuration file for a company.
-- TODO: merge user field into QWCConfig record?
companyQwcConfig :: AppConfig -> Company -> QWCFile
companyQwcConfig cfg c = QWCFile
    ( QWCConfig
        { qcAppDescription     = "Syncing Accounts to " <> appName cfg
        , qcAppDisplayName     = Nothing
        , qcAppID              = "QBFC_AS"
        , qcAppName            = appName cfg <> " - " <> companyName c
        , qcAppSupport         = url "/support/"
        , qcAppUniqueName      = Nothing
        , qcAppURL             = url $ appBaseUrl cfg <> "/accountSync/"
        , qcCertURL            = Just $ url $ appBaseUrl cfg <> "/cert/"
        , qcAuthFlags          = []
        , qcFileID             = appAccountSyncID cfg
        , qcIsReadOnly         = False
        , qcNotify             = False
        , qcOwnerID            = appAccountSyncID cfg
        , qcPersonalDataPref   = Nothing
        , qcQBType             = Financial
        , qcScheduler          = Just $ EveryMinute $ appAccountSyncInterval cfg
        , qcStyle              = Nothing
        , qcUnattendedModePref = Nothing
        }
    , companyUser c
    )
  where
    url path = T.concat
        [ "https://"
        , appHostname cfg
        , if appPort cfg /= 80 then ":" <> pack (show $ appPort cfg) else ""
        , path
        ]
