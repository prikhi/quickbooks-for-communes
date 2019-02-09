{-# LANGUAGE OverloadedStrings #-}
module Main where

import App (app)
import Network.Wai (requestBody)
import Network.Wai.Handler.Warp (setPort, setHost, defaultSettings)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)

main :: IO ()
main = runTLS
    (tlsSettings "cert.pem" "key.pem")
    (setHost "*" $ setPort 3000 $ defaultSettings)
    $ logAllMiddleware app

logAllMiddleware app_ req respond = do
    print req
    print =<< requestBody req
    app_ req respond
