-- | Testing module to play with logging.

module Main where

import           Universum

import           Data.Monoid      ((<>))
import           Data.Yaml.Pretty (defConfig, encodePretty)

import           System.Wlog      (CanLog, Severity (Debug), buildAndSetupYamlLogging,
                                   dispatchEvents, logDebug, logError, logInfo, logNotice,
                                   logWarning, modifyLoggerName, parseLoggerConfig,
                                   prefixB, productionB, releaseAllHandlers, runPureLog,
                                   severityB, usingLoggerName)

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

showPureLog :: IO ()
showPureLog = do
    (res, pureLog) <- runPureLog testLogging
    putText "Pure log:"
    usingLoggerName "naked" $ do
        logWarning $ "Pure log for result = " <> show res <> ":"
        dispatchEvents pureLog

main :: IO ()
main = do
    testToJsonConfigOutput
    let config = (productionB <> prefixB "logs" <> severityB Debug)
    bracket_
        (buildAndSetupYamlLogging config testLoggerConfigPath)
        releaseAllHandlers
        (testLogging >> showPureLog)
