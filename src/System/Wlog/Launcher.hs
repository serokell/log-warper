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

import           Control.Concurrent         (modifyMVar_)
import           Control.Error.Util         ((?:))
import           Control.Exception          (throwIO)
import qualified Data.HashMap.Strict        as HM hiding (HashMap)
import           Data.List                  (isSuffixOf)
import qualified Data.Text                  as T
import           Data.Yaml                  (decodeFileEither)
import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            ((</>))
import           Universum

import           System.Wlog.CanLog         (memoryLogs)
import           System.Wlog.Formatter      (stdoutFormatter, stdoutFormatterTimeRounded)
import           System.Wlog.Handler        (LogHandler (setFormatter))
import           System.Wlog.Handler.Simple (fileHandler)
import           System.Wlog.Logger         (addHandler, updateGlobalLogger)
import           System.Wlog.LoggerConfig   (LoggerConfig (..), LoggerTree (..))
import           System.Wlog.LoggerName     (LoggerName (..))
import           System.Wlog.MemoryQueue    (newMemoryQueue)
import           System.Wlog.Roller         (rotationFileHandler)
import           System.Wlog.Wrapper        (Severity (Debug), initTerminalLogging,
                                             setSeverityMaybe)

data HandlerFabric
    = forall h . LogHandler h => HandlerFabric (FilePath -> Severity -> IO h)

-- | This function traverses 'LoggerConfig' initializing all subloggers
-- with 'Severity' and redirecting output in file handlers.
-- See 'LoggerConfig' for more details.
setupLogging :: MonadIO m => LoggerConfig -> m ()
setupLogging LoggerConfig{..} = do
    liftIO $ createDirectoryIfMissing True handlerPrefix
    when consoleOutput $ initTerminalLogging isShowTime _lcTermSeverity
    -- TODO: use lifted version here
    whenJust _lcMemModeLimit $ \limit -> do
        putText "Initializing logs"
        let cpj = const . pure . Just -- just for lulz
        liftIO $ modifyMVar_ memoryLogs $ cpj $ (newMemoryQueue limit, _lcRoundVal)
    processLoggers mempty _lcTree
  where
    handlerPrefix = _lcFilePrefix ?: "."
    logMapper     = appEndo _lcMapper
    isShowTime    = getAny _lcShowTime
    consoleOutput = getAny _lcConsoleOutput

    handlerFabric :: HandlerFabric
    handlerFabric = case _lcRotation of
        Nothing  -> HandlerFabric fileHandler
        Just rot -> HandlerFabric $ rotationFileHandler rot

    processLoggers :: MonadIO m => LoggerName -> LoggerTree -> m ()
    processLoggers parent LoggerTree{..} = do
        -- This prevents logger output to appear in terminal
        unless (parent == mempty && not consoleOutput) $
            setSeverityMaybe parent _ltSeverity

        forM_ _ltFiles $ \fileName -> liftIO $ do
            let fileSeverity   = _ltSeverity ?: Debug
            let handlerPath    = handlerPrefix </> fileName
            case handlerFabric of
                HandlerFabric fabric -> do
                    let handlerCreator = fabric handlerPath fileSeverity
                    let defFmt = (`setFormatter` stdoutFormatter isShowTime)
                    let roundFmt r = (`setFormatter` stdoutFormatterTimeRounded r)
                    let fmt = maybe defFmt (\r -> if ".pub" `isSuffixOf` fileName
                                                  then roundFmt r
                                                  else defFmt) _lcRoundVal
                    thisLoggerHandler <- fmt <$> handlerCreator
                    updateGlobalLogger (loggerName parent) $ addHandler thisLoggerHandler

        for_ (HM.toList _ltSubloggers) $ \(name, loggerConfig) -> do
            let thisLoggerName = LoggerName $ T.unpack name
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
    setupLogging builtConfig

-- | Initialize logger hierarchy from configuration file.
-- See this module description.
initLoggingFromYaml :: MonadIO m => FilePath -> m ()
initLoggingFromYaml = buildAndSetupYamlLogging mempty
