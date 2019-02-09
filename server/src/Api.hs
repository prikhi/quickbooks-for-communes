{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Api where

import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import           QBWC                           ( QWCConfig )
import           Servant.API                    ( (:>)
                                                , (:<|>)
                                                , Get
                                                , PlainText
                                                , NoContent
                                                )
import           XML                            ( XML )

type API =
    "qwc" :> Get '[XML] (QWCConfig, Text)
    :<|> "accountSync" :> Get '[PlainText] NoContent

api :: Proxy API
api = Proxy
