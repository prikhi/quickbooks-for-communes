{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{- | This module contains a "SOAP" content type for Servant Routes that
assumes that requests & responses are simple XML embedded in SOAP @Envelope@
& @Body@ elements.
-}
module SOAP where

import           Control.Monad.Catch.Pure       ( SomeException
                                                , runCatch
                                                , throwM
                                                )
import           Data.Bifunctor                 ( first )
import           Data.Typeable                  ( Typeable )
import qualified Network.HTTP.Media            as M
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
    mimeUnrender _ bs = either (Left . show) (parseSoapBody . documentRoot)
        $ parseLBS def bs

-- | Render the type as XML with a SOAP Envelope/Body wrapper.
instance (ToXML a) => MimeRender SOAP a where
    mimeRender _ = xrender . doc defaultDocInfo . soapWrapper . toXML


-- | Parse a type out of XML embedded in a SOAP @Envelope@ and @Body@.
parseSoapBody :: FromXML a => Element -> Either String a
parseSoapBody e = first show . runCatch $ parseEnvelope e
  where
    parseEnvelope el =
        if elementName el
            == "{http://schemas.xmlsoap.org/soap/envelope/}Envelope"
        then
            case elementNodes el of
                [NodeElement bodyEl] -> parseBody bodyEl
                _ ->
                    throwM (error "Invalid Envelope Contents" :: SomeException)
        else
            throwM (error "Missing SOAP Envelope" :: SomeException)
    parseBody el =
        if elementName el == "{http://schemas.xmlsoap.org/soap/envelope/}Body"
            then case elementNodes el of
                [NodeElement contentsEl] -> fromXML contentsEl
                _ -> throwM (error "Invalid Body Contents" :: SomeException)
            else throwM (error "Missing SOAP Body" :: SomeException)



-- | Wrap generic XML with SOAP @Envelope@ and @Body@ elements.
soapWrapper :: Xml Elem -> Xml Elem
soapWrapper el =
    xelemQ soapNamespace "Envelope" $ xelemQ soapNamespace "Body" el


-- | The XML namespace for SOAP envelopes. Uses the @soap@ XML prefix.
soapNamespace :: Namespace
soapNamespace = namespace "soap" "http://schemas.xmlsoap.org/soap/envelope/"
