{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}

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
-- > rotation:                # [optional] parameters for logging rotation
-- >     logLimit: 1024       # max size of log file in bytes
-- >     keepFiles: 3         # number of files with logs to keep including current one
-- > node:                    # logger named «node»
-- >     severity: Warning    # severity for logger «node»
-- >     comm:                # logger named «node.comm»
-- >         severity: Info   # severity for logger «node.comm»
-- >         file: patak.jpg  # messages will be also printed to patak.jpg
--
-- And this configuration corresponds two loggers with 'LoggerName'`s
-- @node@ and @node.comm@.

module System.Wlog.Launcher
       ( buildAndSetupYamlLogging
       , initLoggingFromYaml
       , parseLoggerConfig
       , setupLogging
       ) where

import           Universum

import           Control.Error.Util            ((?:))
import           Control.Exception             (throwIO)
import qualified Data.HashMap.Strict           as HM hiding (HashMap)
import           Data.Time                     (UTCTime)
import           Data.Yaml                     (decodeFileEither)
import           System.Directory              (createDirectoryIfMissing)
import           System.FilePath               ((</>))

import           System.Wlog.Formatter         (centiUtcTimeF, stdoutFormatter,
                                                stdoutFormatterTimeRounded)
import           System.Wlog.IOLogger          (addHandler, setPrefix, setSeveritiesMaybe,
                                                updateGlobalLogger)
import           System.Wlog.LoggerConfig      (HandlerWrap (..), LoggerConfig (..),
                                                LoggerTree (..))
import           System.Wlog.LoggerName        (LoggerName (..))
import           System.Wlog.LogHandler        (LogHandler (setFormatter))
import           System.Wlog.LogHandler.Roller (rotationFileHandler)
import           System.Wlog.LogHandler.Simple (fileHandler)
import           System.Wlog.Severity          (Severities, debugPlus, severityPlus)
import           System.Wlog.Terminal          (initTerminalLogging)

data HandlerFabric
    = forall h . LogHandler h => HandlerFabric (FilePath -> Severities -> IO h)

-- | This function traverses 'LoggerConfig' initializing all subloggers
-- with 'Severity' and redirecting output in file handlers.
-- See 'LoggerConfig' for more details.
setupLogging :: MonadIO m => Maybe (UTCTime -> Text) -> LoggerConfig -> m ()
setupLogging mTimeFunction LoggerConfig{..} = do
    liftIO $ createDirectoryIfMissing True handlerPrefix

    whenJust consoleAction $ \customTerminalAction ->
        initTerminalLogging timeF
                            customTerminalAction
                            isShowTime
                            isShowTid
                            (_lcTermSeverity >>= pure . severityPlus)

    liftIO $ setPrefix _lcFilePrefix
    processLoggers mempty _lcTree
  where
    handlerPrefix = _lcFilePrefix ?: "."
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
            setSeveritiesMaybe parent (_ltSeverity >>= pure . severityPlus)

        forM_ _ltFiles $ \HandlerWrap{..} -> liftIO $ do
            let fileSeverities   = (_ltSeverity >>= pure . severityPlus) ?: debugPlus
            let handlerPath    = handlerPrefix </> _hwFilePath
            case handlerFabric of
                HandlerFabric fabric -> do
                    let handlerCreator = fabric handlerPath fileSeverities
                    let defFmt = (`setFormatter` stdoutFormatter timeF isShowTime isShowTid)
                    let roundFmt r = (`setFormatter` stdoutFormatterTimeRounded timeF r)
                    let fmt = maybe defFmt roundFmt _hwRounding
                    thisLoggerHandler <- fmt <$> handlerCreator
                    updateGlobalLogger parent $ addHandler thisLoggerHandler

        for_ (HM.toList _ltSubloggers) $ \(name, loggerConfig) -> do
            let thisLoggerName = LoggerName name
            let thisLogger     = parent <> logMapper thisLoggerName
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

-- | Initialize logger hierarchy from configuration file.
-- See this module description.
initLoggingFromYaml :: MonadIO m => FilePath -> m ()
initLoggingFromYaml = buildAndSetupYamlLogging mempty
