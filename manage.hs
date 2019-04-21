#!/usr/bin/env stack
{- stack
    script
    --ghc-options -threaded
    --ghc-options -Wall
    --ghc-options -Wcompat
    --ghc-options -Wincomplete-record-updates
    --ghc-options -Wincomplete-uni-patterns
    --ghc-options -Wredundant-constraints
    --resolver lts-13.17
    --package ansi-terminal
    --package async
    --package directory
    --package text
    --package typed-process
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{- | This script manages building our project. It is a stack script, so it
does not require a local stack project. You can run it with @stack script
manage.hs@ or @./manage.hs@.

To build the server & client for production, run @./manage.hs build@. To
watch the source files and automatically build & run the server and client,
run @./manage.hs watch@. To remove all the build artifacts, run
@./manage.hs clean@. When run without an argument, the script will make
a production build.

Note: @pscid@ will be installed with the client dependencies, but you need
@ghcid@ on your path. The simplest way is to run @stack install ghcid@
& make sure @~/.local/bin/@ is in your @$PATH@.


TODO:
* Review manage.hs script for SESE website & turn common code into a package.
* Currently pscid steals all input, see how to avoid this. Setting it's
  stdin to closed or a pipe makes it not work :/ Open issues about that.
* What's needed to run ghcid as a "server" we can parse the output of
  & print successes, warnings, & errors?
* ghcid & pscid like to clear the screen or output many newlines, clean
  that up or make feature requests.
* Use a logging framework instead of directly printing.
* Use @nvm@ for client commands.

-}

module Main where

import           Control.Concurrent.Async       ( async
                                                , waitAnyCancel
                                                )
import           Control.Monad                  ( void
                                                , when
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           GHC.IO.Handle                  ( BufferMode(LineBuffering)
                                                , Handle
                                                , hSetBuffering
                                                , hIsEOF
                                                )
import           GHC.IO.Handle.FD               ( stdout
                                                , stderr
                                                )
import           System.Environment             ( getArgs )
import           System.Console.ANSI            ( Color(..)
                                                , ColorIntensity(Vivid)
                                                , ConsoleIntensity
                                                    ( BoldIntensity
                                                    )
                                                , ConsoleLayer(Foreground)
                                                , SGR(..)
                                                , setSGR
                                                )
import           System.Exit                    ( ExitCode(..)
                                                , exitFailure
                                                )
import           System.Process.Typed           ( Process
                                                , createPipe
                                                , setWorkingDir
                                                , setStdout
                                                , setStderr
                                                , getStdout
                                                , getStderr
                                                , proc
                                                , withProcess
                                                , waitExitCode
                                                )


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    getArgs >>= \case
        []        -> watch
        ["build"] -> build
        ["watch"] -> watch
        ["clean"] -> clean
        _ -> printLog Failed "Valid Commands are `watch`, `build` & `clean`"



-- Commands

-- | Make a production build.
build :: MonadIO m => m ()
build = clean >> initClient >> buildClient >> initServer >> buildServer

-- | Start the frontend & backend build servers.
watch :: MonadIO m => m ()
watch = do
    initClient >> initServer
    void . liftIO $ do
        printLog Info "Starting Client Builder"
        clientBuild <- async
            $ run "yarn" ["run", "watch"] "./client/" clientOutput
        printLog Info "Starting Client Dev Server"
        clientServe <- async
            $ run "yarn" ["run", "serve"] "./client/" clientOutput
        printLog Info "Starting Server Builder"
        server <- async $ run
            "ghcid"
            [ "--command"
            , "stack ghci"
            , "--run"
            , "--reload=settings.yaml"
            , "--restart=stack.yaml"
            , "--restart=package.yaml"
            , "--restart=default-settings.yaml"
            , "--color=always"
            ]
            "./server/"
            serverOutput
        waitAnyCancel [clientBuild, clientServe, server]

-- | Remove all the build artifacts & output.
clean :: MonadIO m => m ()
clean = do
    printLog Info "Removing Client Files"
    void $ run
        "rm"
        [ "-r"
        , "-f"
        , "output"
        , "node_modules"
        , "generated-docs"
        , ".cache"
        , "dist"
        , ".spago"
        ]
        "./client/"
        clientOutput
    printLog Info "Removing Server Files"
    void $ run "rm" ["-r", "-f", ".stack-work"] "./server/" serverOutput
    printLog Success "Project Cleaned"



-- Initialize / Build

-- | Install GHC & build the server dependencies.
initServer :: MonadIO m => m ()
initServer = do
    installDependency "stack"
                      ["setup", "--color", "always"]
                      "./server/"
                      serverOutput
                      "GHC"
    installDependency
        "stack"
        [ "build"
        , "--only-dependencies"
        , "--haddock"
        , "--ghc-options"
        , "-O2"
        , "--ghc-options"
        , "-threaded"
        , "--color"
        , "always"
        ]
        "./server/"
        serverOutput
        "Server Dependencies"

-- | Build the server for production.
buildServer :: MonadIO m => m ()
buildServer = do
    printLog Info "Building Server"
    run
            "stack"
            [ "build"
            , "--pedantic"
            , "--haddock"
            , "--ghc-options"
            , "-O2"
            , "--ghc-options"
            , "-threaded"
            , "--color"
            , "always"
            ]
            "./server/"
            serverOutput
        >>= \case
                ExitSuccess -> printLog Success "Server Build Completed"
                ExitFailure _ ->
                    printLog Failed "Server Build Failed" >> liftIO exitFailure

-- | Install the NPM dependencies & the Purescript dependencies.
initClient :: MonadIO m => m ()
initClient = do
    installDependency "yarn"
                      ["install"]
                      "./client/"
                      clientOutput
                      "Javascript Dependencies"
    installDependency "yarn"
                      ["run", "spago", "install"]
                      "./client/"
                      clientOutput
                      "Purescript Dependencies"

-- | Build the client code using the package's @build@ script.
buildClient :: MonadIO m => m ()
buildClient = do
    printLog Info "Building Client"
    run "yarn" ["run", "build"] "./client/" clientOutput >>= \case
        ExitSuccess -> printLog Success "Client Build Completed"
        ExitFailure _ ->
            printLog Failed "Client Build Failed" >> liftIO exitFailure



-- General Runners

-- | Install a dependency.
installDependency
    :: MonadIO m
    => FilePath
    -- ^ Command
    -> [String]
    -- ^ Arguments
    -> String
    -- ^ Working Directory
    -> (Handle -> IO a)
    -- ^ Output Handler
    -> T.Text
    -- ^ Dependency Description
    -> m ()
installDependency cmd args workingDir outputLogger description = do
    printLog Info $ "Installing " <> description
    run cmd args workingDir outputLogger >>= printExitStatus
        (description <> " Installed")
        (description <> " Installation Failed")

-- | Run a command with arguments in a specific directory, printing out the
-- stdout & stderr with the given logger.
run
    :: MonadIO m
    => FilePath
    -- ^ The command
    -> [String]
    -- ^ It's arguments
    -> String
    -- ^ The directory to run the command in
    -> (Handle -> IO a)
    -- ^ The output logger
    -> m ExitCode
    -- ^ The result of running the command
run cmd args workingDir outputLogger =
    let processConfig =
            setWorkingDir workingDir
                $ setStdout createPipe
                $ setStderr createPipe
                $ proc cmd args
    in  liftIO $ withProcess processConfig withOutputLogger
  where
    -- Run the output logger on the stderr & stdin.
    withOutputLogger :: Process stdin Handle Handle -> IO ExitCode
    withOutputLogger p = do
        void $ outputLogger (getStdout p)
        void $ outputLogger (getStderr p)
        waitExitCode p



-- Logging

-- | Potential message types for logging output. These get assigned colors
-- and surrounded with brackets when we print them out.
data LogType
    = Info
    | Failed
    | Success
    | Client
    | Server
    deriving (Show, Read, Eq, Enum, Bounded)

-- | What's the maximum length of LogType's 'show' strings.
maxLogLength :: Int
maxLogLength =
    succ $ maximum $ map (length . show) [minBound .. maxBound :: LogType]

-- | How much padding to add to a LogType to make it the 'maxLogLength'.
paddingLength :: LogType -> Int
paddingLength logType = maxLogLength - length (show logType)

-- | Print the LogType in brackets followed by a message.
printLog :: MonadIO m => LogType -> T.Text -> m ()
printLog logType message =
    liftIO $ printInBrackets logType >> T.putStrLn message

-- | Print out the output of a server command.
serverOutput :: MonadIO m => Handle -> m ()
serverOutput = prependOutput Server

-- | Print out the output of a client command.
clientOutput :: MonadIO m => Handle -> m ()
clientOutput = prependOutput Client

-- | Print out each line of output from the handle prepended with
-- a LogType.
prependOutput :: MonadIO m => LogType -> Handle -> m ()
prependOutput logType handle =
    liftIO
        .   void
        .   async
        .   whileM_ (not <$> hIsEOF handle)
        $   T.hGetLine handle
        >>= printLog logType
  where
    whileM_ :: Monad m => m Bool -> m () -> m ()
    whileM_ predicateM actionM = do
        predicate <- predicateM
        when predicate $ actionM >> whileM_ predicateM actionM

-- | Print different text depending on a process's ExitCode.
printExitStatus
    :: MonadIO m
    => T.Text
    -- ^ The success message.
    -> T.Text
    -- ^ The failure message.
    -> ExitCode
    -> m ()
printExitStatus successText errorText = \case
    ExitSuccess   -> printLog Success successText
    ExitFailure _ -> printLog Failed errorText

-- | Print the LogType in bold colors & surrounded by brackets. Does not
-- include a newline.
printInBrackets :: MonadIO m => LogType -> m ()
printInBrackets logType = liftIO $ do
    T.putStr "["
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid color]
    T.putStr text
    setSGR [Reset]
    T.putStr $ "]" <> T.replicate padding " "
  where
    color :: Color
    color = case logType of
        Info    -> Blue
        Success -> Green
        Failed  -> Red
        Client  -> Cyan
        Server  -> Magenta
    text :: T.Text
    text = T.toUpper . T.pack $ show logType
    padding :: Int
    padding = paddingLength logType
