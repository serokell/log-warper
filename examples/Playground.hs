-- | Testing module to play with logging.

module Main where

import           Control.Exception (bracket_)

import           Data.Monoid       ((<>))
import qualified Data.Text         as T (unlines)
import qualified Data.Text.IO      as TIO (putStrLn)
import           System.Log.Logger (removeAllHandlers)

import           System.Wlog       (CanLog, acquirePureLog, initLoggingFromYaml, logDebug,
                                    logError, logInfo, logNotice, logWarning,
                                    modifyLoggerName, usingLoggerName)

testLogging :: CanLog m => m ()
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
    pureLog <- acquirePureLog testLogging
    TIO.putStrLn "Pure log:"
    TIO.putStrLn $ T.unlines pureLog

main :: IO ()
main = do
    bracket_
        (initLoggingFromYaml "logger-config-example.yaml" $ Just "logs")
        removeAllHandlers
        testLogging

    showPureLog
