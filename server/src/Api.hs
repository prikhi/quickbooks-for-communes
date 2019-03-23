{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Api where

import           Data.Aeson                     ( (.=)
                                                , ToJSON(..)
                                                , object
                                                )
import           Data.ByteString                ( ByteString )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( decodeUtf8 )
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
                                                , ReqBody
                                                , PlainText
                                                , JSON
                                                , NoContent
                                                )
import           SOAP                           ( SOAP )
import           Text.XML.Generator             ( xrender )
import           XML                            ( XML
                                                , ToXML(..)
                                                )

-- | Represents the API presented by our server.
type API =
    FrontendAPI :<|> QuickBooksAPI

-- | The API for communication with the Frontend.
type FrontendAPI =
    "qwc" :> Get '[JSON, XML] QWCFile

-- | The API for the communication with the QuickBooks WebConnector.
type QuickBooksAPI =
         "cert" :> Get '[PlainText] NoContent
    :<|> "accountSync" :> ReqBody '[SOAP] Callback :> Post '[SOAP] CallbackResponse

api :: Proxy API
api = Proxy


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
