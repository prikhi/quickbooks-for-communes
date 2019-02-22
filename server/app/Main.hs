{-# LANGUAGE OverloadedStrings #-}
module Main where

import           App                            ( AppEnv(..)
                                                , app
                                                )
import           Config                         ( AppConfig(..)
                                                , loadConfig
                                                , buildConnectionString
                                                )
import           Control.Monad.Logger           ( runStdoutLoggingT )
import           Database.Persist.Postgresql    ( createPostgresqlPool
                                                , runSqlPool
                                                , runMigration
                                                )
import           DB.Schema                      ( migrateAll )
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
    cfg    <- loadConfig ["settings.yaml"]
    dbPool <- runStdoutLoggingT $ createPostgresqlPool
        (buildConnectionString cfg)
        (appDBConnectionCount cfg)
    flip runSqlPool dbPool $ runMigration migrateAll
    let env = AppEnv cfg dbPool
        runner = if appEnableTLS cfg then runTLS tlsSettings else runSettings
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
    runner settings $ logStdoutDev $ app env
