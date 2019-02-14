{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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

instance Accept SOAP where
    contentType _ = "text" M.// "xml" M./: ("charset", "utf-8")

instance (FromXML a) => MimeUnrender SOAP a where
    mimeUnrender _ bs = either (Left . show) (parseSoapBody . documentRoot)
        $ parseLBS def bs

instance (ToXML a) => MimeRender SOAP a where
    mimeRender _ = xrender . doc defaultDocInfo . soapWrapper . toXML


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



soapWrapper :: Xml Elem -> Xml Elem
soapWrapper el =
    xelemQ soapNamespace "Envelope" $ xelemQ soapNamespace "Body" el


soapNamespace :: Namespace
soapNamespace = namespace "soap" "http://schemas.xmlsoap.org/soap/envelope/"
