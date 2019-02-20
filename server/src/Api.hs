{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Api where

import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import           QuickBooks.QBXML               ( Callback
                                                , CallbackResponse
                                                )
import           QuickBooks.WebConnector        ( QWCConfig )
import           Servant.API                    ( (:>)
                                                , (:<|>)
                                                , Get
                                                , Post
                                                , ReqBody
                                                , PlainText
                                                , NoContent
                                                )
import           SOAP                           ( SOAP )
import           XML                            ( XML )

-- | Represents the API presented by our server.
type API =
         "qwc" :> Get '[XML] (QWCConfig, Text)
    :<|> "cert" :> Get '[PlainText] NoContent
    :<|> "accountSync" :> ReqBody '[SOAP] Callback :> Post '[SOAP] CallbackResponse

api :: Proxy API
api = Proxy
