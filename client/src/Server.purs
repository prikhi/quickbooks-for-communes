module Server where

import Prelude

import Affjax (Response, ResponseFormatError, post, get, printResponseFormatError)
import Affjax.RequestBody as Request
import Affjax.ResponseFormat as Response
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode ((.:), class DecodeJson, decodeJson)
import Data.Argonaut.Encode ((:=), (~>), class EncodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either)
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
        pure $ AccountData { id, name, description }


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
