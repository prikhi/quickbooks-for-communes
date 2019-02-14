{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Api where

import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import           QBXML                          ( Callback
                                                , CallbackResponse
                                                )
import           QBWC                           ( QWCConfig )
import           Servant.API                    ( (:>)
                                                , (:<|>)
                                                , Get
                                                , Post
                                                , ReqBody
                                                )
import           SOAP                           ( SOAP )
import           XML                            ( XML )

type API =
         "qwc" :> Get '[XML] (QWCConfig, Text)
    :<|> "accountSync" :> ReqBody '[SOAP] Callback :> Post '[SOAP] CallbackResponse

api :: Proxy API
api = Proxy
