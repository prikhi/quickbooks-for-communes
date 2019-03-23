module Server where

import Prelude

import Affjax (Response, post_)
import Affjax.RequestBody as Request
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode ((:=), (~>), class EncodeJson, encodeJson)
import Halogen as H

import App (AppM)

class Monad m <= Server m where
    newCompanyRequest :: NewCompanyData -> m (Response Unit)

instance serverApp :: Server AppM where
    newCompanyRequest = liftAff <<< ncRequest

instance serverHalogen :: Server m => Server (H.HalogenM s f g o p m) where
    newCompanyRequest = H.lift <<< newCompanyRequest

-- | Handle NewCompany POST requests.
-- TODO: abstract out Endpoint type when we have multiple server routes
ncRequest :: NewCompanyData -> Aff (Response Unit)
ncRequest ncData =
    post_ "/api/new-company/" $ Request.json $ encodeJson ncData


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
