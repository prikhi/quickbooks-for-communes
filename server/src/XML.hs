{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module XML where

import           Data.Typeable                  ( Typeable )
import qualified Network.HTTP.Media            as M
import           Servant.API                    ( Accept(..)
                                                , MimeRender(..)
                                                )
import           Text.XML.Generator             ( Xml
                                                , Doc
                                                , xrender
                                                )

class ToXML a where
    toXML :: a -> Xml Doc

data XML deriving Typeable

instance Accept XML where
    contentType _ = "application" M.// "xml" M./: ("charset", "utf-8")

instance (ToXML a) => MimeRender XML a where
    mimeRender _ = xrender . toXML
