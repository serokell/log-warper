-- | Testing module to play with logging.

module Main where

import Universum

import Data.Monoid ((<>))
import Data.Yaml.Pretty (defConfig, encodePretty)

import System.Wlog (CanLog, buildAndSetupYamlLogging, logDebug, logError, logInfo, logNotice,
                    logWarning, modifyLoggerName, parseLoggerConfig, prefixB, productionB,
                    removeAllHandlers, usingLoggerName)

testLoggerConfigPath :: FilePath
testLoggerConfigPath = "logger-config-example.yaml"

testToJsonConfigOutput :: MonadIO m => m ()
testToJsonConfigOutput = do
    cfg             <- parseLoggerConfig testLoggerConfigPath
    let builtConfig  = cfg <> productionB <> prefixB "logs"
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

showSomeLog :: IO ()
showSomeLog = do
    putText "Other log:"
    usingLoggerName "naked" $ do
        logWarning "Some warning"
        logDebug   "Some debug"

main :: IO ()
main = do
    testToJsonConfigOutput
    let config = (productionB <> prefixB "logs")
    bracket_
        (buildAndSetupYamlLogging config testLoggerConfigPath)
        removeAllHandlers
        (testLogging >> showSomeLog)
