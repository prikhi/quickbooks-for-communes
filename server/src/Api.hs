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
import           Data.Text                      ( Text )
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


-- | Data describing a Company's 'Account's.
data AccountData
    = AccountData
        { adAccountId :: AccountId
        , adAccountName :: Text
        , adAccountDescription :: Text
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
