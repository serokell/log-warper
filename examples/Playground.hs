-- | Testing module to play with logging.

module Main where

import Universum

import Data.Yaml.Pretty (defConfig, encodePretty)

import System.Wlog (CanLog, defaultConfig, launchFromFile, launchSimpleLogging, logDebug, logError,
                    logInfo, logNotice, logWarning, modifyLoggerName, parseLoggerConfig,
                    productionB, usingLoggerName)

testLoggerConfigPath :: FilePath
testLoggerConfigPath = "logger-config-example.yaml"

testToJsonConfigOutput :: MonadIO m => m ()
testToJsonConfigOutput = do
    cfg             <- parseLoggerConfig testLoggerConfigPath
    let builtConfig  = cfg <> productionB
    putStrLn $ encodePretty defConfig builtConfig

testLogging :: (CanLog m) => m ()
testLogging = usingLoggerName "node" $ do
    logDebug   "skovoroda"
    logInfo    "patak"
    logNotice  "boroda"
    logWarning "haha"

    modifyLoggerName (<> "server") $ do
        logDebug  "provoda"
        logNotice "Ggurda"

    logError   "BARDAQ"

showSomeLog :: (CanLog m, MonadIO m) => m ()
showSomeLog = do
    putTextLn "Other log:"
    usingLoggerName "naked" $ do
        logWarning "Some warning"
        logDebug   "Some debug"

main :: IO ()
main = do
    testToJsonConfigOutput
    let runPlayLog = testLogging >> showSomeLog

    putStrLn $ encodePretty defConfig $ defaultConfig "example"
    putTextLn "Default configurations.."
    launchSimpleLogging "node" runPlayLog

    putTextLn "\nFrom file configurations.."
    launchFromFile testLoggerConfigPath "node" runPlayLog
