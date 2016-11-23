-- | Testing module to play with logging.

module Main where

import           Control.Exception (bracket_)
import           System.Log.Logger (removeAllHandlers)

import           System.Wlog       (initLoggingFromYaml, logDebug, logError, logInfo,
                                    logNotice, logWarning, usingLoggerName)
main :: IO ()
main = bracket_ (initLoggingFromYaml "logger-config-example.yaml" $ Just "logs")
                removeAllHandlers
                testLogging
  where
    testLogging = usingLoggerName "node" $ do
        logDebug   "skovoroda"
        logInfo    "patak"
        logNotice  "boroda"
        logWarning "haha"
        logError   "BARDAQ"
