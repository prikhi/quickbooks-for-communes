{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Api where

import           Data.Aeson                     ( (.=)
                                                , FromJSON
                                                , ToJSON(..)
                                                , object
                                                )
import           Data.ByteString                ( ByteString )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( decodeUtf8 )
import           DB.Schema                      ( CompanyId )
import           GHC.Generics                   ( Generic )
import           QuickBooks.WebConnector        ( QWCConfig
                                                , Callback
                                                , CallbackResponse
                                                , generateConnectorFile
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
import           Text.XML.Generator             ( xrender )
import qualified Validation                    as V
import           XML                            ( XML
                                                , ToXML(..)
                                                )

-- | Represents the API presented by our server.
type API =
    FrontendAPI :<|> QuickBooksAPI

-- | The API for communication with the Frontend.
type FrontendAPI =
         "new-company" :> ReqBody '[JSON] NewCompany :> Post '[JSON] QWCFile
    :<|> "qwc" :> Capture "companyid" CompanyId :> Get '[JSON, XML] QWCFile

-- | The API for the communication with the QuickBooks WebConnector.
type QuickBooksAPI =
         "cert" :> Get '[PlainText] NoContent
    :<|> "accountSync" :> ReqBody '[SOAP] Callback :> Post '[SOAP] CallbackResponse

api :: Proxy API
api = Proxy


data NewCompany
    = NewCompany
        { ncName :: Text
        , ncUsername :: Text
        , ncPassword :: Text
        } deriving (Read, Show, Generic)

instance FromJSON NewCompany

instance V.AppValidation NewCompany where
    validator NewCompany {..} =
        NewCompany
            <$> V.isNonEmpty "name" ncName
            <*> V.isNonEmpty "username" ncUsername
            <*> V.isNonEmpty "password" ncPassword


newtype QWCFile = QWCFile (QWCConfig, Text) deriving (Generic)

instance ToXML QWCFile where
    toXML (QWCFile (c, u)) = generateConnectorFile c u

instance ToJSON QWCFile where
    toJSON f =
        let
            xml :: ByteString
            xml = xrender $ toXML f
        in
            object [ "config" .= decodeUtf8 xml ]
