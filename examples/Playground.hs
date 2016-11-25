-- | Testing module to play with logging.

module Main where

import           Control.Exception (bracket_)

import           Data.Monoid       ((<>))
import qualified Data.Text         as T (pack)
import qualified Data.Text.IO      as TIO (putStrLn)
import           System.Log.Logger (removeAllHandlers)

import           System.Wlog       (CanLog, initLoggingFromYaml, logDebug, logError,
                                    logInfo, logNotice, logWarning, modifyLoggerName,
                                    runPureLog, usingLoggerName)

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
    (res, pureLog) <- runPureLog testLogging
    TIO.putStrLn $ "Pure log for result = " <> (T.pack $ show res) <> ":"
    TIO.putStrLn pureLog

main :: IO ()
main = do
    bracket_
        (initLoggingFromYaml "logger-config-example.yaml" $ Just "logs")
        removeAllHandlers
        testLogging

    showPureLog
