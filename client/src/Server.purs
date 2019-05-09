module Server where

import Prelude

import Affjax (Response, ResponseFormatError, post, get, printResponseFormatError)
import Affjax.RequestBody as Request
import Affjax.ResponseFormat as Response
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode ((.:), class DecodeJson, decodeJson)
import Data.Argonaut.Encode ((:=), (~>), class EncodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.MediaType (MediaType(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Halogen as H
import Web.File.Blob as Blob

import App (AppM)
import Validation as V


-- | Make requests to the API server.
-- |
-- | TODO: Use or build own RemoteData type? Maybe with seperate constructors
-- | for server errors vs client network/decoding errors?
class Monad m <= Server m where
    -- | Fetch the data for all available Companies.
    companiesRequest :: m (Either String (Array CompanyData))
    -- | Fetch the Trip Advances & Store Accounts for a Company.
    companyAccountsRequest :: Int -> m (Either String CompanyAccounts)
    -- | Update the Trip Advances & Store Acounts for a Company.
    editCompanyRequest :: Int -> EditCompanyData -> m (Response (Either V.FormErrors EmptyJson))
    -- | Fetch the StoreAccounts for the New Trip Form.
    tripStoreRequest :: Int -> m (Either String (Array TripStoreAccount))
    -- | Fetch the Account data for a Company.
    accountsRequest :: Int -> m (Either String (Array AccountData))
    -- | Add a new Company.
    newCompanyRequest :: NewCompanyData -> m (Response (Either V.FormErrors QWCFile))

instance serverApp :: Server AppM where
    companiesRequest = liftAff cdRequest
    companyAccountsRequest = liftAff <<< caRequest
    editCompanyRequest cId = liftAff <<< ecdRequest cId
    tripStoreRequest = liftAff <<< tsaRequest
    accountsRequest = liftAff <<< adRequest
    newCompanyRequest = liftAff <<< ncRequest

instance serverHalogen :: Server m => Server (H.HalogenM s f g o m) where
    companiesRequest = H.lift companiesRequest
    companyAccountsRequest = H.lift <<< companyAccountsRequest
    editCompanyRequest cId = H.lift <<< editCompanyRequest cId
    tripStoreRequest = H.lift <<< tripStoreRequest
    accountsRequest = H.lift <<< accountsRequest
    newCompanyRequest = H.lift <<< newCompanyRequest


-- CompanyData Request

cdRequest :: Aff (Either String (Array CompanyData))
cdRequest =
    decodeResponse <$> get Response.json "/api/companies/"

data CompanyData
    = CompanyData
        { id :: Int
        , name :: String
        }
derive instance genericCompanyData :: Generic CompanyData _

instance showCompanyData :: Show CompanyData where
    show = genericShow

instance decodeCompanyData :: DecodeJson CompanyData where
    decodeJson json = do
        o <- decodeJson json
        id <- o .: "cdCompanyId"
        name <- o .: "cdCompanyName"
        pure $ CompanyData { id, name }

ecdRequest :: Int -> EditCompanyData -> Aff (Response (Either V.FormErrors EmptyJson))
ecdRequest cId ecData =
    V.handleResponseErrors
        <$> post Response.string ("/api/edit-company/" <> show cId <> "/")
                (Request.json $ encodeJson ecData)


data EditCompanyData
    = EditCompany
        { tripAdvances :: Maybe Int
        , storeAccounts :: Array StoreAccount
        }
derive instance genericEditCompanyData :: Generic EditCompanyData _

instance showEditCompanyData :: Show EditCompanyData where
    show = genericShow

instance encodeEditCompanyData :: EncodeJson EditCompanyData where
    encodeJson (EditCompany c) =
           "ecTripAdvances" := c.tripAdvances
        ~> "ecStoreAccounts" := c.storeAccounts
        ~> jsonEmptyObject

-- | A type for ignored JSON responses. We use this instead of Unit because
-- | Unit's DecodeJson instance expects a `null` value.
data EmptyJson = EmptyJson

-- | Always return an EmptyJson, no matter what structure the JSON is.
instance decodeEmptyJson :: DecodeJson EmptyJson where
    decodeJson = const $ Right EmptyJson


-- CompanyAccounts Request

caRequest :: Int -> Aff (Either String CompanyAccounts)
caRequest id =
    decodeResponse <$> get Response.json ("/api/company-accounts/" <> show id <> "/")

data CompanyAccounts
    = CompanyAccounts
        { tripAdvances :: Maybe AccountData
        , storeAccounts :: Array StoreAccount
        }
derive instance genericCompanyAccounts :: Generic CompanyAccounts _

instance showCompanyAccounts :: Show CompanyAccounts where
    show = genericShow

instance decodeCompanyAccounts :: DecodeJson CompanyAccounts where
    decodeJson json = do
        o <- decodeJson json
        tripAdvances <- o .: "caTripAdvances"
        storeAccounts <- o .: "caStoreAccounts"
        pure $ CompanyAccounts { tripAdvances, storeAccounts }

data StoreAccount
    = StoreAccount
    { name :: String
    , account :: Int
    }
derive instance genericStoreAccount :: Generic StoreAccount _

instance showStoreAccount :: Show StoreAccount where
    show = genericShow

instance decodeStoreAccount :: DecodeJson StoreAccount where
    decodeJson json = do
        o <- decodeJson json
        name <- o .: "saName"
        account <- o .: "saAccount"
        pure $ StoreAccount { name, account }

instance encodeStoreAccount :: EncodeJson StoreAccount where
    encodeJson (StoreAccount acc) =
           "saName" := acc.name
        ~> "saAccount" := acc.account
        ~> jsonEmptyObject


-- Trip Store Accounts Request

tsaRequest :: Int -> Aff (Either String (Array TripStoreAccount))
tsaRequest companyId =
    decodeResponse <$> get Response.json ("/api/store-accounts/" <> show companyId)

data TripStoreAccount
    = TripStoreAccount
        { id :: Int
        , name :: String
        }
derive instance genericTripStoreAccount :: Generic TripStoreAccount _

instance showTripStoreAccount :: Show TripStoreAccount where
    show = genericShow

instance eqTripStoreAccount :: Eq TripStoreAccount where
    eq = genericEq

instance decodeTripStoreAccount :: DecodeJson TripStoreAccount where
    decodeJson json = do
        o <- decodeJson json
        id <- o .: "tsaId"
        name <- o .: "tsaName"
        pure $ TripStoreAccount { name, id }


-- AccountData Request

adRequest :: Int -> Aff (Either String (Array AccountData))
adRequest companyId =
    decodeResponse <$> get Response.json ("/api/accounts/" <> show companyId)

data AccountData
    = AccountData
        { id :: Int
        , name :: String
        , description :: String
        , accountType :: AccountType
        }
derive instance genericAccountData :: Generic AccountData _

instance showAccountData :: Show AccountData where
    show = genericShow

instance eqAccountData :: Eq AccountData where
    eq = genericEq

instance decodeAccountData :: DecodeJson AccountData where
    decodeJson json = do
        o <- decodeJson json
        id <- o .: "adAccountId"
        name <- o .: "adAccountName"
        description <- o .: "adAccountDescription"
        accountType <- o .: "adAccountType"
        pure $ AccountData { id, name, description, accountType }

data AccountType
    = AccountsPayable
    | AccountsReceivable
    | Bank
    | CostOfGoodsSold
    | CreditCard
    | Equity
    | Expense
    | FixedAsset
    | Income
    | LongTermLiability
    | NonPosting
    | OtherAsset
    | OtherCurrentAsset
    | OtherCurrentLiability
    | OtherExpense
    | OtherIncome

derive instance genericAccountType :: Generic AccountType _

instance showAccountType :: Show AccountType where
    show = genericShow

instance eqAccountType :: Eq AccountType where
    eq = genericEq

instance decodeAccountType :: DecodeJson AccountType where
    decodeJson json = do
        decodeJson json >>= case _ of
            "AccountsPayable" -> pure AccountsPayable
            "AccountsReceivable" -> pure AccountsReceivable
            "Bank" -> pure Bank
            "CostOfGoodsSold" -> pure CostOfGoodsSold
            "CreditCard" -> pure CreditCard
            "Equity" -> pure Equity
            "Expense" -> pure Expense
            "FixedAsset" -> pure FixedAsset
            "Income" -> pure Income
            "LongTermLiability" -> pure LongTermLiability
            "NonPosting" -> pure NonPosting
            "OtherAsset" -> pure OtherAsset
            "OtherCurrentAsset" -> pure OtherCurrentAsset
            "OtherCurrentLiability" -> pure OtherCurrentLiability
            "OtherExpense" -> pure OtherExpense
            "OtherIncome" -> pure OtherIncome
            s -> Left $ "Unexpected AccountType String: " <> s

prettyAccountType :: AccountType -> String
prettyAccountType = case _ of
    AccountsPayable -> liability
    AccountsReceivable -> asset
    Bank -> asset
    CostOfGoodsSold -> costOfGoods
    CreditCard -> liability
    Equity -> equity
    Expense -> expense
    FixedAsset -> asset
    Income -> income
    LongTermLiability -> liability
    NonPosting -> nonPosting
    OtherAsset -> asset
    OtherCurrentAsset -> asset
    OtherCurrentLiability -> liability
    OtherExpense -> expense
    OtherIncome -> income
  where
    asset = "Asset"
    costOfGoods = "Cost of Goods Sold"
    equity = "Equity"
    expense = "Expense"
    income = "Income"
    liability = "Liability"
    nonPosting = "Non Posting"




decodeResponse :: forall a
    . DecodeJson a
   => Response (Either ResponseFormatError Json)
   -> Either String a
decodeResponse resp =
    lmap printResponseFormatError resp.body >>= decodeJson



-- New Company Request

-- | Handle NewCompany POST requests.
-- TODO: abstract out Endpoint type when we have multiple server routes
ncRequest
    :: NewCompanyData
    -> Aff (Response (Either V.FormErrors QWCFile))
ncRequest ncData =
    V.handleResponseErrors
        <$> post Response.string "/api/new-company/" (Request.json $ encodeJson ncData)

data NewCompanyData
    = NewCompany
        { name :: String
        , user :: String
        , password :: String
        }

instance encodeNewCompany :: EncodeJson NewCompanyData where
    encodeJson (NewCompany c) =
           "ncName" := c.name
        ~> "ncUsername" := c.user
        ~> "ncPassword" := c.password
        ~> jsonEmptyObject

newtype QWCFile = QWCFile Blob.Blob

instance decodeQWCFile :: DecodeJson QWCFile where
    decodeJson json = do
        o <- decodeJson json
        qwcText <- o .: "config"
        pure $ QWCFile $ Blob.fromString qwcText $ MediaType "text/xml"
