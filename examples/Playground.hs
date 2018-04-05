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

import System.Wlog (CanLog, atLogger, consoleActionB, defaultConfig, getLogger, infoPlus,
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

testLogging :: (CanLog m, MonadIO m) => m ()
testLogging = usingLoggerName "node" $ do
    logDebug   "skovoroda"
    logInfo    "patak"
    logNotice  "boroda"
    logWarning "haha"

    getLogger "node" >>= \l -> logDebug $ show l
    getLogger "node.server" >>= \l -> logDebug $ show l
    getLogger "node.missing" >>= \l -> logDebug $ show l

    modifyLoggerName (<> "server") $ do
        logDebug  "provoda"
        logNotice "Ggurda"
        modifyLoggerName (<> "missing") $ do
            logInfo "should be in node.server"

    modifyLoggerName (<> "missing") $ do
        logInfo "should be in node"

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

    putTextLn "Default configurations with modification.."
    launchWithConfig (defaultConfig "node" & atLogger "node" . ltSeverity ?~ infoPlus)
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
concurrentActions = logWarningWaitInf 2 "stupid action" someStupidAction
  where
    someStupidAction :: m ()
    someStupidAction = replicateM_ 10 $ do
      threadDelay $ sec 1
      liftIO $ putTextLn "HEYYEYAAEYAAAEYAEYAA"
#endif
