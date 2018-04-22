{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Testing module to play with logging.

module Main where

import Universum

import Data.Yaml (FromJSON, ToJSON, decodeFileEither, prettyPrintParseException)
import Data.Yaml.Pretty (defConfig, encodePretty)

import Log (Configuration, Extension (..), LogCtx (..), RotationExtension, Severity (..),
            WithLogger, launchLogger, logM, logStdout, withSublogger)

testLoggerConfigPath :: FilePath
testLoggerConfigPath = "log-warper/logger-config-example.yaml"

testToJsonConfigOutput :: IO ()
testToJsonConfigOutput = do
    testConfig @'[]
    testConfig @'[ 'Rotation ]
  where
    testConfig :: forall (exts :: [Extension]) .
                  (FromJSON (RotationExtension exts), ToJSON (RotationExtension exts))
               => IO ()
    testConfig = decodeFileEither @(Configuration exts) testLoggerConfigPath >>= \case
        Left err  -> putStrLn $ prettyPrintParseException err
        Right cfg -> putStrLn $ encodePretty defConfig cfg


-- testLogging :: (CanLog m) => m ()
-- testLogging = usingLoggerName "node" $ do
--     logDebug   "debug"
--     logInfo    "info"
--     logNotice  "notice"
--     logWarning "warning"
--
--     modifyLoggerName (<> "server") $ do
--         logDebug  "server-debug"
--         logInfo   "server-info"
--         logNotice "server-warning"
--         modifyLoggerName (<> "missing") $ do
--             logInfo "should be in node.server"
--
--     modifyLoggerName (<> "missing") $ do
--         logInfo "should be in node"
--
--     logError   "BARDAQ"
--

showSomeLog :: WithLogger exts m => m ()
showSomeLog = do
    logM Debug   "Some debug"
    logM Info    "Some info"
    logM Notice  "Some notice"
    logM Warning "Some warning"
    logM Error   "Some error"

    withSublogger "nested" $ do
        logM Debug   "Some nested debug"
        logM Info    "Some nested info"
        logM Notice  "Some nested notice"
        logM Warning "Some nested warning"
        logM Error   "Some nested error"

main :: IO ()
main = do
    testToJsonConfigOutput

    config <- fromRight (error "Parse error")
          <$> decodeFileEither @(Configuration '[ 'Rotation ]) testLoggerConfigPath

    launchLogger logStdout (LogCtx "app" config) showSomeLog

--     let runPlayLog = testLogging >> showSomeLog
--
--     putTextLn "Default configurations with modification.."
--     launchWithConfig (defaultConfig "node" & atLogger "node" . ltSeverity ?~ infoPlus
--                                            & atLogger "node.server" . ltSeverity ?~ debugPlus)
--                      "node"
--                      runPlayLog
--
--     putTextLn "\nFrom file configurations.."
--     launchFromFile testLoggerConfigPath "node" runPlayLog
--
--     putTextLn "\nShould be silent..."
--     launchWithConfig (defaultConfig "node" <> consoleActionB (\_ _ -> return ()))
--                      "node"
--                      runPlayLog
--
-- #if ( __GLASGOW_HASKELL__ >= 802 )
--     putTextLn "\nConcurrent..."
--     launchSimpleLogging "concurrent" concurrentActions
--
-- concurrentActions :: forall m . (WithLoggerIO m, MonadBaseControl IO m) => m ()
-- concurrentActions = logWarningWaitInf 2 "stupid action" someStupidAction
--   where
--     someStupidAction :: m ()
--     someStupidAction = replicateM_ 10 $ do
--       threadDelay $ sec 1
--       liftIO $ putTextLn "HEYYEYAAEYAAAEYAEYAA"
-- #endif
