{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- | API routes for the Frontend.
-}
module Routes.Frontend
    ( routes
    , companies
    , accounts
    , newCompany
    , generateQwcFile
    )
where

import           Api                            ( FrontendAPI
                                                , CompanyData(..)
                                                , EditCompanyData(..)
                                                , CompanyAccountsData(..)
                                                , StoreAccountData(..)
                                                , TripStoreAccount(..)
                                                , AccountData(..)
                                                , NewCompany(..)
                                                )
import           Config                         ( AppConfig(..) )
import           Control.Exception.Safe         ( MonadThrow
                                                , throw
                                                )
import           Control.Monad                  ( (>=>)
                                                , forM_
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                )
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( pack )
import qualified Data.Text                     as T
import           Database.Persist.Sql           ( (==.)
                                                , (<-.)
                                                , (/<-.)
                                                , (=.)
                                                , Entity(..)
                                                , SelectOpt(..)
                                                , selectList
                                                , insert_
                                                , get
                                                , getBy
                                                , getEntity
                                                , update
                                                , upsertBy
                                                , deleteWhere
                                                )
import           DB.Schema                      ( Company(..)
                                                , CompanyId
                                                , Account(..)
                                                , AccountId
                                                , StoreAccount(..)
                                                , Unique(..)
                                                , EntityField(..)
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
import           Utils                          ( traverseWithIndex )
import qualified Validation                    as V


-- | The assembled handlers for the 'FrontendAPI' type.
routes :: ServerT FrontendAPI AppM
routes =
    companies
        :<|> editCompany
        :<|> companyAccounts
        :<|> tripStoreAccounts
        :<|> accounts
        :<|> newCompany
        :<|> generateQwcFile


-- | Fetch all Companies.
companies :: (SqlDB m) => m [CompanyData]
companies = runDB $ map convert <$> selectList [] [Desc CompanyName]
  where
    convert :: Entity Company -> CompanyData
    convert (Entity cId c) =
        CompanyData {cdCompanyId = cId, cdCompanyName = companyName c}


-- | Update a Company's TripAdvances & StoreAccounts.
--
-- TODO: Require user account via cookie auth
editCompany :: (SqlDB m, MonadThrow m) => CompanyId -> EditCompanyData -> m ()
editCompany companyId = V.validateOrThrow >=> \EditCompany {..} -> runDB $ do
    let submittedAccounts =
            maybe [] (flip (:) []) ecTripAdvances
                <> map saAccount ecStoreAccounts
    validAccounts <-
        map entityKey
            <$> selectList
                    [ AccountId <-. submittedAccounts
                    , AccountCompany ==. companyId
                    ]
                    []
    let
        validationTest =
            ()
                <$ validateTripAdvance validAccounts ecTripAdvances
                <* traverseWithIndex (validateStoreAccount validAccounts)
                                     ecStoreAccounts
    V.whenValid validationTest $ \_ -> do
        update companyId [CompanyTripAdvances =. ecTripAdvances]
        deleteWhere
            [ StoreAccountCompany ==. companyId
            , StoreAccountAccount /<-. map saAccount ecStoreAccounts
            ]
        forM_ ecStoreAccounts $ \StoreAccountData {..} -> upsertBy
            (UniqueStoreAccount saAccount companyId)
            (StoreAccount
                { storeAccountName    = saName
                , storeAccountAccount = saAccount
                , storeAccountCompany = companyId
                }
            )
            [StoreAccountName =. saName]
  where
    validateTripAdvance
        :: [AccountId] -> Maybe AccountId -> V.Validation V.FormErrors ()
    validateTripAdvance accs = \case
        Nothing        -> pure ()
        Just tripAdvId -> if tripAdvId `notElem` accs
            then V.validate
                "trip-advances"
                "Could not find this Account in the selected Company."
                (const True)
                ()
            else pure ()
    validateStoreAccount
        :: [AccountId]
        -> Int
        -> StoreAccountData
        -> V.Validation V.FormErrors AccountId
    validateStoreAccount accs index storeAccount =
        V.validate ("store-account-" <> T.pack (show index) <> "-account")
                   "Could not find this Account in the selected Company."
                   (`elem` accs)
            $ saAccount storeAccount


-- | Fetch the TripAdvances Account & All Store Accounts for a Company.
companyAccounts :: SqlDB m => CompanyId -> m CompanyAccountsData
companyAccounts companyId = do
    result <- runDB $ do
        companyAccs   <- get companyId
        storeAccounts <- selectList [StoreAccountCompany ==. companyId]
                                    [Asc StoreAccountName]
        case companyAccs of
            Just company -> do
                maybeTripAdvance <- case companyTripAdvances company of
                    Nothing        -> return Nothing
                    Just tripAdvId -> getEntity tripAdvId
                return (maybeTripAdvance, storeAccounts)
            Nothing -> return (Nothing, storeAccounts)
    return $ convert result
  where
    convert
        :: (Maybe (Entity Account), [Entity StoreAccount])
        -> CompanyAccountsData
    convert (maybeTripAdvance, storeAccounts) = CompanyAccountsData
        { caTripAdvances  = fmap toAccountData maybeTripAdvance
        , caStoreAccounts = map convertStore storeAccounts
        }
    convertStore :: Entity StoreAccount -> StoreAccountData
    convertStore (Entity _ acc) = StoreAccountData
        { saName    = storeAccountName acc
        , saAccount = storeAccountAccount acc
        }


-- | Return the StoreAccount's for a Company's Trip form.
tripStoreAccounts :: SqlDB m => CompanyId -> m [TripStoreAccount]
tripStoreAccounts companyId = runDB $ map convert <$> selectList
    [StoreAccountCompany ==. companyId]
    [Asc StoreAccountName]
  where
    convert :: Entity StoreAccount -> TripStoreAccount
    convert (Entity saId sa) =
        TripStoreAccount {tsaId = saId, tsaName = storeAccountName sa}


-- | Fetch the Accounts for a 'Company'.
accounts :: SqlDB m => CompanyId -> m [AccountData]
accounts companyId = runDB $ map toAccountData <$> selectList
    [AccountCompany ==. companyId, AccountIsActive ==. True]
    [Asc AccountName]

toAccountData :: Entity Account -> AccountData
toAccountData (Entity aId a) = AccountData
    { adAccountId          = aId
    , adAccountName        = accountName a
    , adAccountDescription = accountDescription a
    , adAccountType        = accountType a
    }


-- | Validate & create a new 'Company'.
--
-- TODO: Require user account via cookie auth
newCompany
    :: (MonadReader AppEnv m, HashPassword m, SqlDB m, MonadThrow m)
    => NewCompany
    -> m QWCConfig
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
                        , companyTripAdvances = Nothing
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
    :: (MonadReader AppEnv m, SqlDB m, MonadThrow m) => CompanyId -> m QWCConfig
generateQwcFile companyId = do
    cfg     <- asks appConfig
    company <- runDB $ get companyId >>= maybe (throw err404) return
    return $ companyQwcConfig cfg company

-- | Build the WebConnetor configuration file for a company.
companyQwcConfig :: AppConfig -> Company -> QWCConfig
companyQwcConfig cfg c = QWCConfig
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
    , qcUserName           = companyUser c
    }
  where
    url path = T.concat
        [ "https://"
        , appHostname cfg
        , if appPort cfg /= 80 then ":" <> pack (show $ appPort cfg) else ""
        , path
        ]
