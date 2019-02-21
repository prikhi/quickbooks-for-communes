{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{- | This module contains the runtime configuration data type as well as
a function for loading
-}
module Config
    ( AppConfig(..)
    , loadConfig
    )
where

import           Control.Exception.Safe         ( try )
import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , withObject
                                                )
import           Data.Streaming.Network         ( HostPreference )
import           Data.String                    ( fromString )
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           Data.Yaml                      ( prettyPrintParseException )
import           Data.Yaml.Config               ( loadYamlSettings
                                                , useEnv
                                                )
import           Paths_qbfc                     ( getDataFileName )
import           System.Exit                    ( exitFailure )


-- | The Configuration Data for the Application. Used when starting the
-- server and for returning constant data from routes.
data AppConfig =
    AppConfig
        { appHost :: HostPreference
        -- ^ The hostname to bind the server to. E.g., @*@ or @localhost@.
        , appHostname :: Text
        -- ^ The hostname that the WebConnector will connect to. E.g.,
        -- @qbfc-server.local@.
        , appPort :: Int
        -- ^ The port the server listens on.

        , appEnableTLS :: Bool
        -- ^ Enable HTTPS connections via TLS. WebConnector requires HTTPS
        -- for servers on different hosts, but if you have an HTTPS proxy
        -- in front of the server, you can disable this.
        , appAllowInsecure :: Bool
        -- ^ Enabling this will still allow HTTP connections when TLS is
        -- enabled.
        , appTLSKeyFile :: FilePath
        -- ^ The path to the TLS key file.
        , appTLSCertFile :: FilePath
        -- ^ The path to the TLS certificate file.
        , appTLSChainFiles :: [FilePath]
        -- ^ Optional paths to your TLS root/intermediate certificate
        -- files.

        , appAccountSyncUsername :: Text
        -- ^ The username to expect when syncing the QUickBooks Accounts.
        , appAccountSyncPassword :: Text
        -- ^ The password to expect when syncing the QUickBooks Accounts.
        , appAccountSyncInterval :: Integer
        -- ^ The number of minutes to wait between syncing the QuickBooks
        -- Accounts.
        , appAccountSyncID :: UUID
        -- ^ The Owner/File ID for the Account Sync WebConnector endpoint.
        }

instance FromJSON AppConfig where
    parseJSON = withObject "AppConfig" $ \o -> do
        host <- fmap fromString $ o .: "host"
        hostname <- o .: "hostname"
        port <- o .: "port"
        (accUsername, accPassword, accSyncInterval, accSyncID) <- o .: "account-sync"
            >>= withObject "account-sync"
                    (\acc ->
                        (,,,)
                            <$> acc .: "username"
                            <*> acc .: "password"
                            <*> acc .: "interval"
                            <*> acc .: "id"
                    )
        (useTLS, allowInsecure, keyFile, certFile, chainFiles) <- o .: "tls"
            >>= withObject "tls"
                    (\tls ->
                        (,,,,)
                            <$> tls .: "enable"
                            <*> tls .: "allow-insecure"
                            <*> tls .: "key-file"
                            <*> tls .: "cert-file"
                            <*> tls .: "cert-chain-files"
                    )
        return AppConfig
            { appHost = host
            , appHostname = hostname
            , appPort = port

            , appEnableTLS = useTLS
            , appAllowInsecure = allowInsecure
            , appTLSKeyFile = keyFile
            , appTLSCertFile = certFile
            , appTLSChainFiles = chainFiles

            , appAccountSyncUsername = accUsername
            , appAccountSyncPassword = accPassword
            , appAccountSyncInterval = accSyncInterval
            , appAccountSyncID = accSyncID
            }

-- | Load the Application Configuration from YAML files, allowing for
-- Environmental Variable overrides.
--
-- If the settings files do not contain all expected keys, the options will
-- fallback to the defaults specified in the @default-settings.yaml@ file.
-- Note that this default settings file does not contain instance-specific
-- data such as the UUIDs for the WebConnector syncing routes.
--
-- If a parsing error occurs, the error will be printed and the application
-- will exit with an error.
loadConfig :: [FilePath] -> IO AppConfig
loadConfig filePaths = do
    defaultSettings <- getDataFileName "default-settings.yaml"
    let settingsFiles = filePaths ++ [defaultSettings]
    try (loadYamlSettings settingsFiles [] useEnv) >>= \case
        Left parseException -> do
            putStrLn $ prettyPrintParseException parseException
            exitFailure
        Right config -> return config
