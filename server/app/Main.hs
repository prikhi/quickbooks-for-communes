{-# LANGUAGE OverloadedStrings #-}
module Main where

import           App                            ( app )
import           Network.Wai.Handler.Warp       ( setPort
                                                , setHost
                                                , defaultSettings
                                                )
import           Network.Wai.Handler.WarpTLS    ( runTLS
                                                , tlsSettings
                                                )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdoutDev )

main :: IO ()
main =
    runTLS (tlsSettings "cert.pem" "key.pem")
           (setHost "*" $ setPort 3000 $ defaultSettings)
        $ logStdoutDev app
