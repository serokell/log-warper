{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : System.Wlog.Parser
-- Copyright   : (c) Serokell, 2016
-- License     : GPL-3 (see the file LICENSE)
-- Maintainer  : Serokell <hi@serokell.io>
-- Stability   : experimental
-- Portability : POSIX, GHC
--
-- Parser for configuring and initializing logger from YAML file.
-- Logger configuration should look like this:
--
-- > rotation:                    # [optional] parameters for logging rotation
-- >     logLimit: 1024           # max size of log file in bytes
-- >     keepFiles: 3             # number of files with logs to keep including current one
-- > loggerTree:
-- >     severity: Warning+       # severities for «root» logger
-- >     node:                    # logger named «node»
-- >         severity: Warning+   # severities for logger «node»
-- >         comm:                # logger named «node.comm»
-- >             severity: Info+  # severity for logger «node.comm»
-- >             file: patak.jpg  # messages will be also printed to patak.jpg
--
-- And this configuration corresponds two loggers with 'LoggerName'`s
-- @node@ and @node.comm@.

module System.Wlog.Launcher
       ( buildAndSetupYamlLogging
       , defaultConfig
       , launchFromFile
       , launchSimpleLogging
       , parseLoggerConfig
       , setupLogging
       ) where

import Universum

import Control.Exception (throwIO)
import Data.Time (UTCTime)
import Data.Yaml (decodeFileEither)
import Lens.Micro.Platform (zoom, (.=), (?=))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import System.Wlog.Formatter (centiUtcTimeF, stdoutFormatter, stdoutFormatterTimeRounded)
import System.Wlog.IOLogger (addHandler, removeAllHandlers, setPrefix, setSeveritiesMaybe,
                             updateGlobalLogger)
import System.Wlog.LoggerConfig (HandlerWrap (..), LoggerConfig (..), LoggerTree (..), fromScratch,
                                 lcConsoleAction, lcShowTime, lcTree, ltSeverity, productionB,
                                 zoomLogger)
import System.Wlog.LoggerName (LoggerName)
import System.Wlog.LoggerNameBox (LoggerNameBox, usingLoggerName)
import System.Wlog.LogHandler (LogHandler (setFormatter))
import System.Wlog.LogHandler.Roller (rotationFileHandler)
import System.Wlog.LogHandler.Simple (defaultHandleAction, fileHandler)
import System.Wlog.Severity (Severities, debugPlus, warningPlus)
import System.Wlog.Terminal (initTerminalLogging)

import qualified Data.HashMap.Strict as HM hiding (HashMap)

data HandlerFabric
    = forall h . LogHandler h => HandlerFabric (FilePath -> Severities -> IO h)

-- | This function traverses 'LoggerConfig' initializing all subloggers
-- with 'Severities' and redirecting output in file handlers.
-- See 'LoggerConfig' for more details.
setupLogging :: MonadIO m => Maybe (UTCTime -> Text) -> LoggerConfig -> m ()
setupLogging mTimeFunction LoggerConfig{..} = do
    liftIO $ createDirectoryIfMissing True handlerPrefix

    whenJust consoleAction $ \customTerminalAction ->
        initTerminalLogging timeF
                            customTerminalAction
                            isShowTime
                            isShowTid
                            _lcTermSeverityOut
                            _lcTermSeverityErr

    liftIO $ setPrefix _lcLogsDirectory
    processLoggers mempty _lcTree
  where
    handlerPrefix = fromMaybe "." _lcLogsDirectory
    logMapper     = appEndo _lcMapper
    timeF         = fromMaybe centiUtcTimeF mTimeFunction
    isShowTime    = getAny _lcShowTime
    isShowTid     = getAny _lcShowTid
    consoleAction = getLast _lcConsoleAction

    handlerFabric :: HandlerFabric
    handlerFabric = case _lcRotation of
        Nothing  -> HandlerFabric fileHandler
        Just rot -> HandlerFabric $ rotationFileHandler rot

    processLoggers :: MonadIO m => LoggerName -> LoggerTree -> m ()
    processLoggers parent LoggerTree{..} = do
        -- This prevents logger output to appear in terminal
        unless (parent == mempty && isNothing consoleAction) $
            setSeveritiesMaybe parent _ltSeverity

        forM_ _ltFiles $ \HandlerWrap{..} -> liftIO $ do
            let fileSeverities   = fromMaybe debugPlus _ltSeverity
            let handlerPath    = handlerPrefix </> _hwFilePath
            case handlerFabric of
                HandlerFabric fabric -> do
                    let handlerCreator = fabric handlerPath fileSeverities
                    let defFmt = (`setFormatter` stdoutFormatter timeF isShowTime isShowTid)
                    let roundFmt r = (`setFormatter` stdoutFormatterTimeRounded timeF r)
                    let fmt = maybe defFmt roundFmt _hwRounding
                    thisLoggerHandler <- fmt <$> handlerCreator
                    updateGlobalLogger parent $ addHandler thisLoggerHandler

        for_ (HM.toList _ltSubloggers) $ \(loggerName, loggerConfig) -> do
            let thisLogger     = parent <> logMapper loggerName
            processLoggers thisLogger loggerConfig

-- | Parses logger config from given file path.
parseLoggerConfig :: MonadIO m => FilePath -> m LoggerConfig
parseLoggerConfig loggerConfigPath =
    liftIO $ join $ either throwIO return <$> decodeFileEither loggerConfigPath

-- | Applies given builder to parsed logger config and initializes logging.
buildAndSetupYamlLogging :: MonadIO m => LoggerConfig -> FilePath -> m ()
buildAndSetupYamlLogging configBuilder loggerConfigPath = do
    cfg@LoggerConfig{..} <- parseLoggerConfig loggerConfigPath
    let builtConfig       = cfg <> configBuilder
    setupLogging Nothing builtConfig

-- | Initializes logging using given 'FilePath' to logger configurations,
-- runs the action with the given 'LoggerName'.
launchFromFile :: (MonadIO m, MonadMask m)
               => FilePath
               -> LoggerName
               -> LoggerNameBox m a
               -> m a
launchFromFile filename loggerName action =
    bracket_
        (buildAndSetupYamlLogging productionB filename)
        removeAllHandlers
        (usingLoggerName loggerName action)

{- | Default logging configuration with the given 'LoggerName'.

Enabled flags:

  - 'ltSeverity' of the root logger is set to 'warningPlus'
    ('System.Wlog.Severity.Warning' and upper)
  - 'ltSeverity' for the given logger is set to 'debugPlus' ('System.Wlog.Severity.Debug' and upper)
  - 'lcShowTime' is set to 'Any True' which means that time is shown in the log messages.
  - 'lcConsoleAction' is set to 'defaultHandleAction' which turns the console output on.

==== __/Example/__
@ defaultConfig "example"@ will produce such configurations:

@
rotation: null
showTid: false
showTime: true
printOutput: true
logTree:
  _ltSubloggers:
    example:
      _ltSubloggers: {}
      _ltSeverity:
      - Debug
      - Info
      - Notice
      - Warning
      - Error
      _ltFiles: []
  _ltSeverity:
  - Warning
  - Error
  _ltFiles: []
termSeveritiesOut: null
filePrefix: null
termSeveritiesErr: null
@

-}
defaultConfig :: LoggerName -> LoggerConfig
defaultConfig loggerName = fromScratch $ do
    lcShowTime      .= Any True
    lcConsoleAction .= Last (Just defaultHandleAction)
    zoom lcTree $ do
        ltSeverity ?= warningPlus
        zoomLogger loggerName $
            ltSeverity ?= debugPlus

{- | Set ups the logging with 'defaultConfig' and runs the action with the given 'LoggerName'.

==== __/Example/__
Here we can see very simple working example of logging:

@
ghci> __:{__
ghci| __launchSimpleLogging "app" $ do__
ghci|     __logDebug "Debug message"__
ghci|     __putStrLn "Usual printing"__
ghci|     __logInfo "Job's done!"__
ghci| __:}__
[app:DEBUG] [2017-12-07 11:25:06.47 UTC] Debug message
Usual printing
[app:INFO] [2017-12-07 11:25:06.47 UTC] Job's done!
@

-}
launchSimpleLogging :: (MonadIO m, MonadMask m)
                    => LoggerName
                    -> LoggerNameBox m a
                    -> m a
launchSimpleLogging loggerName action =
    bracket_
        (setupLogging Nothing $ defaultConfig loggerName)
        removeAllHandlers
        (usingLoggerName loggerName action)
