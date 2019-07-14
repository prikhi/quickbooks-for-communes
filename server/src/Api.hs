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
    , TripStoreAccount(..)
    , AccountData(..)
    , NewCompany(..)
    , NewTrip(..)
    , NewTripStop(..)
    , NewTripTransaction(..)
    , NewTripCreditStop(..)
    , NewTripCreditTransaction(..)
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
import           Data.Time                      ( UTCTime )
import           Database.Persist.Sql           ( fromSqlKey )
import           DB.Fields                      ( AccountTypeField
                                                , Cents
                                                , Percentage
                                                )
import           DB.Schema                      ( CompanyId
                                                , AccountId
                                                , StoreAccountId
                                                , TripId
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
    :<|> "store-accounts" :> Capture "companyId" CompanyId :> Get '[JSON] [TripStoreAccount]
    :<|> "accounts" :> Capture "companyid" CompanyId :> Get '[JSON] [AccountData]
    :<|> "new-company" :> ReqBody '[JSON] NewCompany :> Post '[JSON] QWCConfig
    :<|> "new-trip" :> Capture "companyId" CompanyId :> ReqBody '[JSON] NewTrip :> Post '[JSON] TripId
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


-- | Data describing a StoreAccount for the Trip Form
data TripStoreAccount
    = TripStoreAccount
        { tsaName :: Text
        , tsaId :: StoreAccountId
        } deriving (Show, Read, Generic)

instance ToJSON TripStoreAccount


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


-- | Post data for creating a new `Trip`.
data NewTrip
    = NewTrip
        { ntDate :: UTCTime
        -- ^ Trip Date
        , ntName :: Text
        -- ^ Tripper Name
        , ntNumber :: Text
        -- ^ Trip Advance Number
        , ntAdvance :: Cents
        -- ^ Trip Advance Amount
        , ntReturned :: Cents
        -- ^ Cash Returned
        , ntStops :: [NewTripStop]
        -- ^ Trip Stops
        , ntCreditStops :: [NewTripCreditStop]
        -- ^ Store Credit Stops
        } deriving (Read, Show, Generic)

instance FromJSON NewTrip

instance V.AppValidation NewTrip where
    validator NewTrip {..} =
        let transactionTotal =
                foldl (\acc stop -> acc + sumTransactions stop) 0 ntStops
        in  NewTrip ntDate
                <$> V.isNonEmpty "name" ntName
                <*> V.isNonEmpty "number" ntNumber
                <*> V.validate "advance" "Must be greater than 0." (> 0) ntAdvance
                <*> V.validate "return" "Must be greater than 0." (> 0) ntReturned
                <*  V.validate "return" "Cannot be greater than advance."
                        (<= ntAdvance) ntReturned
                <*  V.validate "" "Trip is out of balance."
                        (== (ntAdvance - ntReturned)) transactionTotal
                <*> V.traverseWithFieldPrefix "trip-stop" ntStops
                <*> pure ntCreditStops
        where
            sumTransactions :: NewTripStop -> Cents
            sumTransactions stop =
                foldl (\acc trans ->
                        let operator = if nttIsReturn trans then (-) else (+)
                        in  acc `operator` nttTotal trans
                    ) 0 $ ntsTransactions stop


data NewTripStop
    = NewTripStop
        { ntsName :: Text
        -- ^ Stop Name
        , ntsTransactions :: [NewTripTransaction]
        -- ^ Stop Transactions
        } deriving (Read, Show, Generic)

instance FromJSON NewTripStop

instance V.AppValidation NewTripStop where
    validator NewTripStop {..} =
        NewTripStop
            <$> V.isNonEmpty "name" ntsName
            <*> pure ntsTransactions


data NewTripTransaction
    = NewTripTransaction
        { nttAccount :: AccountId
        -- ^ Transaction Account
        , nttMemo :: Text
        -- ^ Item Description
        , nttAmount :: Maybe Cents
        -- ^ Purchase Price
        , nttTax :: Maybe Percentage
        -- ^ Tax as Decimal
        , nttTotal :: Cents
        -- ^ Total Cost
        , nttIsReturn :: Bool
        -- ^ Returning Item?
        } deriving (Show, Read, Generic)

instance FromJSON NewTripTransaction


data NewTripCreditStop
    = NewTripCreditStop
        { ntcsStore :: StoreAccountId
        -- ^ Store Account
        , ntcsTransactions :: [NewTripCreditTransaction]
        -- ^ Credit Transactions
        } deriving (Show, Read, Generic)

instance FromJSON NewTripCreditStop

data NewTripCreditTransaction
    = NewTripCreditTransaction
        { ntctAccount :: AccountId
        -- ^ Transaction Account
        , ntctMemo :: Text
        -- ^ Item Details
        , ntctAmount :: Maybe Cents
        -- ^ Item Price
        , ntctTax :: Maybe Percentage
        -- ^ Tax as Decimal
        , ntctTotal :: Cents
        -- ^ Total Cost
        } deriving (Show, Read, Generic)

instance FromJSON NewTripCreditTransaction


-- QuickBooks

-- | The API for the communication with the QuickBooks WebConnector.
type QuickBooksAPI =
         "cert" :> Get '[PlainText] NoContent
    :<|> "accountSync" :> ReqBody '[SOAP] Callback :> Post '[SOAP] CallbackResponse
