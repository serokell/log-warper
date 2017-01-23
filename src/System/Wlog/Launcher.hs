{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns              #-}

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
-- > node:                    # logger named «node»
-- >     severity: Warning    # severity for logger «node»
-- >     comm:                # logger named «node.comm»
-- >         severity: Info   # severity for logger «node.comm»
-- >         file: patak.jpg  # messages will be also printed to patak.jpg
--
-- And this configuration corresponds two loggers with 'LoggerName'`s
-- @node@ and @node.comm@.

module System.Wlog.Launcher
       ( initLoggingFromYaml
       , initLoggingFromYamlWithMapper
       , parseLoggerConfig
       , traverseLoggerConfig
       ) where

#if PatakDebugSkovorodaBARDAQ
import qualified Data.ByteString.Char8     as BS (putStrLn)
import           Data.Yaml.Pretty          (defConfig, encodePretty)
#endif

import           Control.Error.Util        ((?:))
import           Control.Exception         (throwIO)
import           Control.Monad             (join)
import           Control.Monad.Extra       (whenJust)
import           Control.Monad.IO.Class    (MonadIO (liftIO))

import           Data.Foldable             (for_)
import qualified Data.HashMap.Strict       as HM hiding (HashMap)
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (unpack)
import           Data.Yaml                 (decodeFileEither)

import           System.Directory          (createDirectoryIfMissing)
import           System.FilePath           ((</>))
import           System.Log                (Priority)
import           System.Log.Handler        (LogHandler)
import           System.Log.Handler.Simple (fileHandler)
import           System.Log.Logger         (addHandler, updateGlobalLogger)

import           System.Wlog.Formatter     (setStdoutFormatter)
import           System.Wlog.LoggerConfig  (LoggerConfig (..), LoggerTree (..),
                                            RotationParameters)
import           System.Wlog.LoggerName    (LoggerName (..))
import           System.Wlog.Roller        (rotationFileHandler)
import           System.Wlog.Wrapper       (Severity (Debug, Warning), convertSeverity,
                                            initLogging, setSeverityMaybe)

data HandlerFabric
    = forall h . LogHandler h => HandlerFabric (FilePath -> Priority -> IO h)

-- | This function traverses 'LoggerConfig' initializing all subloggers
-- with 'Severity' and redirecting output in file handlers. Also takes
-- 'FilePath' prefix to create directory for file handlers output. Create
-- logging rotation if appropriate arguments are passed.
traverseLoggerConfig
    :: MonadIO m
    => (LoggerName -> LoggerName)  -- ^ mapper to transform logger names during traversal
    -> Maybe RotationParameters    -- ^ rotation parameters for roller handle
    -> LoggerTree                  -- ^ given 'LoggerConfig' to traverse
    -> Maybe FilePath              -- ^ prefix for file handlers
    -> m ()
traverseLoggerConfig logMapper mrot tree (fromMaybe "." -> handlerPrefix) = do
    liftIO $ createDirectoryIfMissing True handlerPrefix
    initLogging Warning
    processLoggers mempty tree
  where
    handlerFabric :: HandlerFabric
    handlerFabric = case mrot of
        Nothing  -> HandlerFabric fileHandler
        Just rot -> HandlerFabric $ rotationFileHandler rot

    processLoggers :: MonadIO m => LoggerName -> LoggerTree -> m ()
    processLoggers parent LoggerTree{..} = do
        setSeverityMaybe parent ltSeverity

        whenJust ltFile $ \fileName -> liftIO $ do
            let filePriority   = convertSeverity $ ltSeverity ?: Debug
            let handlerPath    = handlerPrefix </> fileName
            case handlerFabric of
              HandlerFabric fabric -> do
                thisLoggerHandler <- setStdoutFormatter True <$> fabric handlerPath filePriority
                updateGlobalLogger (loggerName parent) $ addHandler thisLoggerHandler

        for_ (HM.toList ltSubloggers) $ \(name, loggerConfig) -> do
            let thisLoggerName = LoggerName $ unpack name
            let thisLogger     = parent <> logMapper thisLoggerName
            processLoggers thisLogger loggerConfig

-- | Parses logger config from given file path.
parseLoggerConfig :: MonadIO m => FilePath -> m LoggerConfig
parseLoggerConfig loggerConfigPath =
    liftIO $ join $ either throwIO return <$> decodeFileEither loggerConfigPath

-- | Initialize logger hierarchy from configuration file.
-- See this module description.
initLoggingFromYamlWithMapper
    :: MonadIO m
    => (LoggerName -> LoggerName)
    -> FilePath
    -> Maybe FilePath
    -> m ()
initLoggingFromYamlWithMapper loggerMapper loggerConfigPath handlerPrefix = do
    cfg@LoggerConfig{..} <- parseLoggerConfig loggerConfigPath

#if PatakDebugSkovorodaBARDAQ
    liftIO $ BS.putStrLn $ encodePretty defConfig cfg
#endif

    traverseLoggerConfig loggerMapper lcRotation lcTree handlerPrefix

initLoggingFromYaml :: MonadIO m => FilePath -> Maybe FilePath -> m ()
initLoggingFromYaml = initLoggingFromYamlWithMapper id
