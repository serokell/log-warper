{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Testing module to play with logging.

module Main where

import Universum

#if ( __GLASGOW_HASKELL__ >= 802 )
import Control.Monad.Trans.Control (MonadBaseControl)
#endif
import Data.Yaml.Pretty (defConfig, encodePretty)
import Lens.Micro ((?~))
#if ( __GLASGOW_HASKELL__ >= 802 )
import Time (sec, threadDelay)
#endif

import System.Wlog (CanLog, atLogger, consoleActionB, debugPlus, defaultConfig, infoPlus,
                    launchFromFile, launchWithConfig, logDebug, logError, logInfo, logNotice,
                    logWarning, ltSeverity, modifyLoggerName, parseLoggerConfig, productionB,
                    usingLoggerName)
#if ( __GLASGOW_HASKELL__ >= 802 )
import System.Wlog (WithLoggerIO, launchSimpleLogging, logWarningWaitInf)
#endif

testLoggerConfigPath :: FilePath
testLoggerConfigPath = "logger-config-example.yaml"

testToJsonConfigOutput :: MonadIO m => m ()
testToJsonConfigOutput = do
    cfg             <- parseLoggerConfig testLoggerConfigPath
    let builtConfig  = cfg <> productionB
    putStrLn $ encodePretty defConfig builtConfig

testLogging :: (CanLog m) => m ()
testLogging = usingLoggerName "node" $ do
    logDebug   "debug"
    logInfo    "info"
    logNotice  "notice"
    logWarning "warning"

    modifyLoggerName (<> "server") $ do
        logDebug  "server-debug"
        logInfo   "server-info"
        logNotice "server-warning"
        modifyLoggerName (<> "missing") $ do
            logInfo "should be in node.server"

    modifyLoggerName (<> "missing") $ do
        logInfo "should be in node"

    logError   "BARDAQ"

showSomeLog :: (CanLog m, MonadIO m) => m ()
showSomeLog = usingLoggerName "naked" $ do
    logDebug   "Some debug"
    logInfo    "Some info"
    logNotice  "Some notice"
    logWarning "Some warning"
    modifyLoggerName (<> "nested") $ do
        logDebug   "Some nested debug"
        logInfo    "Some nested info"
        logNotice  "Some nested notice"
        logWarning "Some nested warning"

main :: IO ()
main = do
    testToJsonConfigOutput
    let runPlayLog = testLogging >> showSomeLog

    putTextLn "Default configurations with modification.."
    launchWithConfig (defaultConfig "node" & atLogger "node" . ltSeverity ?~ infoPlus
                                           & atLogger "node.server" . ltSeverity ?~ debugPlus)
                     "node"
                     runPlayLog

    putTextLn "\nFrom file configurations.."
    launchFromFile testLoggerConfigPath "node" runPlayLog

    putTextLn "\nShould be silent..."
    launchWithConfig (defaultConfig "node" <> consoleActionB (\_ _ -> return ()))
                     "node"
                     runPlayLog

#if ( __GLASGOW_HASKELL__ >= 802 )
    putTextLn "\nConcurrent..."
    launchSimpleLogging "concurrent" concurrentActions

concurrentActions :: forall m . (WithLoggerIO m, MonadBaseControl IO m) => m ()
concurrentActions = logWarningWaitInf (sec 2) "stupid action" someStupidAction
  where
    someStupidAction :: m ()
    someStupidAction = replicateM_ 10 $ do
      threadDelay $ sec 1
      liftIO $ putTextLn "HEYYEYAAEYAAAEYAEYAA"
#endif
