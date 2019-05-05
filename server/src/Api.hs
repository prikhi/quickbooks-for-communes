{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{- | A description of the API the application serves, as well as API
specific models/types.
-}
module Api
    ( API
    , api
    -- * Frontend Types
    , FrontendAPI
    , CompanyData(..)
    , EditCompanyData(..)
    , CompanyAccountsData(..)
    , StoreAccountData(..)
    , AccountData(..)
    , NewCompany(..)
    -- * QuickBooks Types
    , QuickBooksAPI
    )
where

import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Database.Persist.Sql           ( fromSqlKey )
import           DB.Fields                      ( AccountTypeField )
import           DB.Schema                      ( CompanyId
                                                , AccountId
                                                )
import           GHC.Generics                   ( Generic )
import           QuickBooks.WebConnector        ( QWCConfig
                                                , Callback
                                                , CallbackResponse
                                                )
import           Servant.API                    ( (:>)
                                                , (:<|>)
                                                , Get
                                                , Post
                                                , Capture
                                                , ReqBody
                                                , PlainText
                                                , JSON
                                                , NoContent
                                                )
import           SOAP                           ( SOAP )
import           Utils                          ( traverseWithIndex )
import qualified Validation                    as V
import           XML                            ( XML )

-- | Represents the API presented by our server.
type API =
    FrontendAPI :<|> QuickBooksAPI

-- | The 'API' as a Proxy value.
api :: Proxy API
api = Proxy



-- Frontend

-- | The API for communication with the Frontend.
type FrontendAPI =
         "companies" :> Get '[JSON] [CompanyData]
    :<|> "edit-company" :> Capture "companyId" CompanyId
            :> ReqBody '[JSON] EditCompanyData :> Post '[JSON] ()
    :<|> "company-accounts" :> Capture "companyId" CompanyId :> Get '[JSON] CompanyAccountsData
    :<|> "accounts" :> Capture "companyid" CompanyId :> Get '[JSON] [AccountData]
    :<|> "new-company" :> ReqBody '[JSON] NewCompany :> Post '[JSON] QWCConfig
    :<|> "qwc" :> Capture "companyid" CompanyId :> Get '[JSON, XML] QWCConfig


-- | Data describing all available 'Company's.
data CompanyData
    = CompanyData
        { cdCompanyId :: CompanyId
        , cdCompanyName :: Text
        } deriving (Show, Read, Generic)

instance ToJSON CompanyData


-- | Data for updating an existing 'Company'.
data EditCompanyData
    = EditCompany
        { ecTripAdvances :: Maybe AccountId
        , ecStoreAccounts :: [StoreAccountData]
        } deriving (Show, Read, Generic)

instance FromJSON EditCompanyData

instance V.AppValidation EditCompanyData where
    validator EditCompany {..} =
        EditCompany ecTripAdvances
            <$> traverseWithIndex validateStoreAccount ecStoreAccounts
      where
        duplicateNames :: Set Text
        duplicateNames = getDuplicates saName ecStoreAccounts
        duplicateAccounts :: Set AccountId
        duplicateAccounts = getDuplicates saAccount ecStoreAccounts
        validateStoreAccount :: Int -> StoreAccountData -> V.Validation V.FormErrors StoreAccountData
        validateStoreAccount index StoreAccountData {..} =
            let field f = "store-account-" <> pack (show index) <> "-" <> f in
            StoreAccountData
                <$> V.isNonEmpty (field "name") saName
                <*  noDuplicates (field "name") "Names" duplicateNames saName
                <*> V.validate (field "account") "An Account is required."
                        ((> 0) . fromSqlKey) saAccount
                <*  noDuplicates (field "account") "Accounts" duplicateAccounts saAccount
        noDuplicates :: Eq b => Text -> Text -> Set b -> b -> V.Validation V.FormErrors b
        noDuplicates field type_ duplicates =
            V.validate field (type_ <> " must be unique.") (`notElem` duplicates)

-- | Get all duplicate values from a list.
getDuplicates :: Ord b => (a -> b) -> [a] -> Set b
getDuplicates selector = snd . foldl
    (\(seen, dupes) item ->
        let val = selector item
        in  if val `Set.member` seen
                then (seen, Set.insert val dupes)
                else (Set.insert val seen, dupes)
    )
    (Set.empty, Set.empty)


-- | Data describing the configured 'Account's for a 'Company'.
data CompanyAccountsData
    = CompanyAccountsData
        { caTripAdvances :: Maybe AccountData
        , caStoreAccounts :: [StoreAccountData]
        } deriving (Show, Read, Generic)

instance ToJSON CompanyAccountsData

-- | Data describing a Company's 'StoreAccount'.
data StoreAccountData
    = StoreAccountData
        { saName :: Text
        , saAccount :: AccountId
        } deriving (Show, Read, Generic)

instance ToJSON StoreAccountData
instance FromJSON StoreAccountData


-- | Data describing a Company's 'Account's.
data AccountData
    = AccountData
        { adAccountId :: AccountId
        , adAccountName :: Text
        , adAccountDescription :: Text
        , adAccountType :: AccountTypeField
        } deriving (Show, Read, Generic)

instance ToJSON AccountData


-- | POST data for creating a new 'Company' model.
data NewCompany
    = NewCompany
        { ncName :: Text
        -- ^ Company Name
        , ncUsername :: Text
        -- ^ Username for WebConnector
        , ncPassword :: Text
        -- ^ Password for WebConnector
        } deriving (Read, Show, Generic)

instance FromJSON NewCompany

-- | Ensure all text is non-empty.
instance V.AppValidation NewCompany where
    validator NewCompany {..} =
        NewCompany
            <$> V.isNonEmpty "name" ncName
            <*> V.isNonEmpty "username" ncUsername
            <*> V.isNonEmpty "password" ncPassword



-- QuickBooks

-- | The API for the communication with the QuickBooks WebConnector.
type QuickBooksAPI =
         "cert" :> Get '[PlainText] NoContent
    :<|> "accountSync" :> ReqBody '[SOAP] Callback :> Post '[SOAP] CallbackResponse
