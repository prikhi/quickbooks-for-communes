module Server where

import Prelude

import Affjax (Response, post)
import Affjax.RequestBody as Request
import Affjax.ResponseFormat as Response
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode ((:=), (~>), class EncodeJson, encodeJson)
import Data.Either (Either)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Halogen as H

import App (AppM)
import Validation as V

class Monad m <= Server m where
    newCompanyRequest :: NewCompanyData -> m (Response (Either V.FormErrors Unit))

instance serverApp :: Server AppM where
    newCompanyRequest = liftAff <<< ncRequest

instance serverHalogen :: Server m => Server (H.HalogenM s f g o p m) where
    newCompanyRequest = H.lift <<< newCompanyRequest

-- | Handle NewCompany POST requests.
-- TODO: abstract out Endpoint type when we have multiple server routes
ncRequest
    :: NewCompanyData
    -> Aff (Response (Either V.FormErrors Unit))
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
