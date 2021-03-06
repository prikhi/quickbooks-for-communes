{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{- | This module contains typeclasses for parsing/generating XML
requests/responses as well as an XML Content-Type for Servant Routes.

Parsing is done with the `xml-conduits` package while generation is done
using `xmlgen`.

-}
module XML where

import           Data.Bifunctor                 ( first )
import           Data.Typeable                  ( Typeable )
import qualified Network.HTTP.Media            as M
import           Servant.API                    ( Accept(..)
                                                , MimeRender(..)
                                                , MimeUnrender(..)
                                                )
import           Text.XML.Generator             ( Xml
                                                , Elem
                                                , xrender
                                                , doc
                                                , defaultDocInfo
                                                )
import           Text.XML                       ( parseLBS_
                                                , def
                                                )

import           Parser                         ( FromXML(..)
                                                , parseDocumentRoot
                                                )

-- | Render a type into XML using the `xmlgen` package.
class ToXML a where
    toXML :: a -> Xml Elem

-- | Create XML requests with xmlgen & parse responses with xml-conduits.
data XML deriving Typeable

-- | Define an XML Servant Content Type.
instance Accept XML where
    contentType _ = "application" M.// "xml" M./: ("charset", "utf-8")

-- | Types implementing ToXML can be returned from Servant XML routes.
instance (ToXML a) => MimeRender XML a where
    mimeRender _ = xrender . doc defaultDocInfo . toXML

-- | Types implementing FromXML can be passed to Servant XML routes.
instance (FromXML a) => MimeUnrender XML a where
    mimeUnrender _ bs = first show $ parseDocumentRoot $ parseLBS_ def bs
