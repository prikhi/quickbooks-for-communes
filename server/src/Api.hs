{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Api where

import Data.Typeable (Typeable, Proxy(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Media as M
import  Servant.API ((:>), (:<|>), Accept(..), MimeRender(..), Get, PlainText, NoContent)

type API =
    "qwc" :> Get '[XML] ByteString
    :<|> "accountSync" :> Get '[PlainText] NoContent

api :: Proxy API
api = Proxy

data XML deriving Typeable

instance Accept XML where
    contentType _ = "application" M.// "xml" M./: ("charset", "utf-8")

instance MimeRender XML ByteString where
    mimeRender _ = L.fromStrict
