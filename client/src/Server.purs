module Server where

import Prelude

import Affjax (Response, ResponseFormatError, post, get, printResponseFormatError)
import Affjax.RequestBody as Request
import Affjax.ResponseFormat as Response
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode ((.:), class DecodeJson, decodeJson)
import Data.Argonaut.Encode ((:=), (~>), class EncodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
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
    -- | Fetch the Account data for a Company.
    accountsRequest :: Int -> m (Either String (Array AccountData))
    -- | Add a new Company.
    newCompanyRequest :: NewCompanyData -> m (Response (Either V.FormErrors QWCFile))

instance serverApp :: Server AppM where
    companiesRequest = liftAff cdRequest
    accountsRequest = liftAff <<< adRequest
    newCompanyRequest = liftAff <<< ncRequest

instance serverHalogen :: Server m => Server (H.HalogenM s f g o m) where
    companiesRequest = H.lift companiesRequest
    accountsRequest = H.lift <<< accountsRequest
    newCompanyRequest = H.lift <<< newCompanyRequest


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
