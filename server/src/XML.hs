{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module XML where

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

-- | Render a type into XML using the `xmlgen` package.
class ToXML a where
    toXML :: a -> Xml Elem

data XML deriving Typeable

instance Accept XML where
    contentType _ = "application" M.// "xml" M./: ("charset", "utf-8")

instance (ToXML a) => MimeRender XML a where
    mimeRender _ = xrender . doc defaultDocInfo . toXML
