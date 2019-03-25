module Server where

import Prelude

import Affjax (Response, post)
import Affjax.RequestBody as Request
import Affjax.ResponseFormat as Response
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode ((.:), class DecodeJson, decodeJson)
import Data.Argonaut.Encode ((:=), (~>), class EncodeJson, encodeJson)
import Data.Either (Either)
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
    newCompanyRequest :: NewCompanyData -> m (Response (Either V.FormErrors QWCFile))

instance serverApp :: Server AppM where
    newCompanyRequest = liftAff <<< ncRequest

instance serverHalogen :: Server m => Server (H.HalogenM s f g o p m) where
    newCompanyRequest = H.lift <<< newCompanyRequest

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
