{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{- | This module contains a "SOAP" content type for Servant Routes that
assumes that requests & responses are simple XML embedded in SOAP @Envelope@
& @Body@ elements.
-}
module SOAP where

import           Data.Bifunctor                 ( first )
import           Data.Text                      ( Text )
import           Data.Typeable                  ( Typeable )
import qualified Network.HTTP.Media            as M
import           Parser                         ( Parser
                                                , runParser
                                                , parseError
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
                                                , documentRoot
                                                , parseLBS
                                                , def
                                                )
import           XML                            ( FromXML(..)
                                                , ToXML(..)
                                                )

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
    mimeUnrender _ bs = either (Left . show) (first show . flip runParser parseFromSoapEnvelope . documentRoot)
        $ parseLBS def bs

-- | Render the type as XML with a SOAP Envelope/Body wrapper.
instance (ToXML a) => MimeRender SOAP a where
    mimeRender _ = xrender . doc defaultDocInfo . soapWrapper . toXML


-- | Parse a type out of XML embedded in a SOAP @Envelope@ and @Body@.
parseFromSoapEnvelope :: FromXML a => Parser a
parseFromSoapEnvelope =
    matchName (soapName "Envelope")
        $   find (soapName "Body")
        $   elementNodes
        <$> getElement
        >>= \case
                [NodeElement bodyContents] -> descend fromXML bodyContents
                _                          -> parseError "Invalid Body Contents"


soapName :: Text -> Name
soapName = withNamespace "http://schemas.xmlsoap.org/soap/envelope/"


-- | Wrap generic XML with SOAP @Envelope@ and @Body@ elements.
soapWrapper :: Xml Elem -> Xml Elem
soapWrapper el =
    xelemQ soapNamespace "Envelope" $ xelemQ soapNamespace "Body" el


-- | The XML namespace for SOAP envelopes. Uses the @soap@ XML prefix.
soapNamespace :: Namespace
soapNamespace = namespace "soap" "http://schemas.xmlsoap.org/soap/envelope/"
