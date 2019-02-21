{-# LANGUAGE OverloadedStrings #-}
module Main where

import           App                            ( app )
import           Config                         ( AppConfig(..)
                                                , loadConfig
                                                )
import           Network.Wai.Handler.Warp       ( setPort
                                                , setHost
                                                , defaultSettings
                                                , runSettings
                                                )
import           Network.Wai.Handler.WarpTLS    ( OnInsecure(..)
                                                , runTLS
                                                , tlsSettingsChain
                                                , onInsecure
                                                )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdoutDev )

main :: IO ()
main = do
    cfg <- loadConfig ["settings.yaml"]
    let runner = if appEnableTLS cfg then runTLS tlsSettings else runSettings
        tlsSettings = (tlsSettingsChain (appTLSCertFile cfg)
                                        (appTLSChainFiles cfg)
                                        (appTLSKeyFile cfg)
                      )
            { onInsecure =
                if appAllowInsecure cfg
                    then AllowInsecure
                    else DenyInsecure
                        "This server only accepts secure HTTPS connections."
            }
        settings =
            setHost (appHost cfg) $ setPort (appPort cfg) defaultSettings
    runner settings $ logStdoutDev app
