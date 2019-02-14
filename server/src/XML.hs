{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module XML where

import           Control.Monad.Catch.Pure       ( MonadThrow
                                                , runCatch
                                                )
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
import           Text.XML                       ( Element
                                                , documentRoot
                                                , parseLBS_
                                                , def
                                                )

-- | Render a type into XML using the `xmlgen` package.
class ToXML a where
    toXML :: a -> Xml Elem

class FromXML a where
    fromXML :: MonadThrow m => Element -> m a

-- | Create XML requests with xmlgen & parse responses with xml-conduits.
data XML deriving Typeable

instance Accept XML where
    contentType _ = "application" M.// "xml" M./: ("charset", "utf-8")

instance (ToXML a) => MimeRender XML a where
    mimeRender _ = xrender . doc defaultDocInfo . toXML

instance (FromXML a) => MimeUnrender XML a where
    mimeUnrender _ bs = first show . runCatch $
        fromXML $ documentRoot $ parseLBS_ def bs
