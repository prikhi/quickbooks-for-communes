{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{- | This module contains a "SOAP" content type for Servant Routes that
assumes that requests & responses are simple XML embedded in SOAP @Envelope@
& @Body@ elements.
-}
module SOAP where

import           Data.Bifunctor                 ( bimap )
import           Data.Text                      ( Text )
import           Data.Typeable                  ( Typeable )
import qualified Network.HTTP.Media            as M
import           Parser                         ( FromXML(..)
                                                , ParsingErrorType(..)
                                                , parseDocumentRoot
                                                , throwParsingError
                                                , getElement
                                                , matchName
                                                , find
                                                , descend
                                                , withNamespace
                                                )
import           Servant.API                    ( Accept(..)
                                                , MimeUnrender(..)
                                                , MimeRender(..)
                                                )
import           Text.XML.Generator             ( Xml
                                                , Elem
                                                , Namespace
                                                , xrender
                                                , xelemQ
                                                , doc
                                                , defaultDocInfo
                                                , namespace
                                                )
import           Text.XML                       ( Node(NodeElement)
                                                , Element(..)
                                                , Name
                                                , parseLBS_
                                                , def
                                                )
import           XML                            ( ToXML(..) )

data SOAP deriving Typeable

-- | Expect SOAP requests using the @text/xml@ Content-Type with a UTF-8
-- Character Set.
--
-- Note: We use @text/xml@ instead of @application/xml@ since that is the
-- Content Type of the WebConnector's POST requests.
instance Accept SOAP where
    contentType _ = "text" M.// "xml" M./: ("charset", "utf-8")

-- | Parse the XML embedded in a SOAP Envelope/Body wrapper.
instance (FromXML a) => MimeUnrender SOAP a where
    mimeUnrender _ bs =
        bimap show fromSOAPRequest . parseDocumentRoot $ parseLBS_ def bs

-- | Render the type as XML with a SOAP Envelope/Body wrapper.
instance (ToXML a) => MimeRender SOAP a where
    mimeRender _ = xrender . doc defaultDocInfo . soapWrapper . toXML


-- | A wrapper for SOAP responses embeded in Envelope & Body elements.
newtype SOAPRequest a
    = SOAPRequest { fromSOAPRequest :: a }

instance FromXML a => FromXML (SOAPRequest a) where
    fromXML = matchName (soapName "Envelope") $ find (soapName "Body") $ do
        el <- getElement
        case elementNodes el of
            [NodeElement bodyContents] -> SOAPRequest <$> descend fromXML bodyContents
            _ -> throwParsingError $ UnexpectedChildNodes "single element"

soapName :: Text -> Name
soapName = withNamespace "http://schemas.xmlsoap.org/soap/envelope/"


-- | Wrap generic XML with SOAP @Envelope@ and @Body@ elements.
soapWrapper :: Xml Elem -> Xml Elem
soapWrapper el =
    xelemQ soapNamespace "Envelope" $ xelemQ soapNamespace "Body" el


-- | The XML namespace for SOAP envelopes. Uses the @soap@ XML prefix.
soapNamespace :: Namespace
soapNamespace = namespace "soap" "http://schemas.xmlsoap.org/soap/envelope/"
