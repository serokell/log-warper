-- |
-- Module      : System.Wlog.Wrapper
-- Copyright   : (c) Serokell, 2016
-- License     : GPL-3 (see the file LICENSE)
-- Maintainer  : Serokell <hi@serokell.io>
-- Stability   : experimental
-- Portability : POSIX, GHC
--
-- Logging functionality. This module is wrapper over
-- <http://hackage.haskell.org/package/hslogger hslogger>,
-- which allows to keep logger name in monadic context.

module System.Wlog.Wrapper
       ( Severity (..)
       , convertSeverity
       , initLogging
       , initLoggingWith
       , setSeverity
       , setSeverityMaybe

         -- * Logging functions
       , logDebug
       , logError
       , logInfo
       , logNotice
       , logWarning
       , logMessage
       ) where

import           Control.Concurrent.MVar   (MVar, newMVar, withMVar)
import           Control.Monad.Trans       (MonadIO (liftIO))

import           Data.Default              (Default (def))
import qualified Data.Text                 as T

import           System.IO                 (Handle, stderr, stdout)
import           System.Log.Handler.Simple (GenericHandler (..), streamHandler)
import           System.Log.Logger         (Priority (DEBUG, ERROR), clearLevel, logM,
                                            rootLoggerName, setHandlers, setLevel,
                                            updateGlobalLogger)

import           System.Wlog.Formatter     (setStderrFormatter, setStdoutFormatter)
import           System.Wlog.LoggerName    (LoggerName (..))
import           System.Wlog.LoggerNameBox (WithNamedLogger (..))
import           System.Wlog.Severity      (Severity (..), convertSeverity)


-- | Options determining formatting of messages.
data LoggingFormat = LoggingFormat
    { -- | Show time for non-error messages.
      -- Note that error messages always have timestamp.
      lfShowTime :: !Bool
    } deriving (Show)

instance Default LoggingFormat where
    def = LoggingFormat {lfShowTime = True}

-- | Like `streamHandler`, but syncronized using given `MVar` as lock
-- (it should be filled before this function call).
streamHandlerWithLock :: MVar () -> Handle -> Priority -> IO (GenericHandler Handle)
streamHandlerWithLock lock h p = do
    GenericHandler{..} <- streamHandler h p
    return GenericHandler
        { priority  = priority
        , formatter = formatter
        , privData  = privData
        , writeFunc = \a s -> withMVar lock $ const $ writeFunc a s
        , closeFunc = closeFunc
        }

-- | This function initializes global logging system. At high level, it sets
-- severity which will be used by all loggers by default, sets default
-- formatters and sets custom severity for given loggers (if any).
--
-- On a lower level it does the following:
-- 1. Removes default handler from root logger, sets two handlers such that:
-- 1.1. All messages are printed to /stdout/.
-- 1.2. Moreover messages with at least `Error` severity are
-- printed to /stderr/.
-- 2. Sets given Severity to root logger, so that it will be used by
-- descendant loggers by default.
-- 3. Applies `setSeverity` to given loggers. It can be done later using
-- `setSeverity` directly.
initLoggingWith
    :: MonadIO m
    => LoggingFormat -> Severity -> m ()
initLoggingWith LoggingFormat {..} defaultSeverity = liftIO $ do
    lock <- liftIO $ newMVar ()
    -- We set DEBUG here, to allow all messages by stdout handler.
    -- They will be filtered by loggers.
    stdoutHandler <- setStdoutFormatter lfShowTime <$>
        streamHandlerWithLock lock stdout DEBUG
    stderrHandler <- setStderrFormatter <$>
        streamHandlerWithLock lock stderr ERROR
    updateGlobalLogger rootLoggerName $
        setHandlers [stderrHandler, stdoutHandler]
    updateGlobalLogger rootLoggerName $
        setLevel (convertSeverity defaultSeverity)

-- | Version of initLoggingWith without any predefined loggers.
initLogging :: MonadIO m => Severity -> m ()
initLogging = initLoggingWith def

-- | Set severity for given logger. By default parent's severity is used.
setSeverity :: MonadIO m => LoggerName -> Severity -> m ()
setSeverity (LoggerName name) =
    liftIO . updateGlobalLogger name . setLevel . convertSeverity

-- | Set or clear severity.
setSeverityMaybe
    :: MonadIO m
    => LoggerName -> Maybe Severity -> m ()
setSeverityMaybe (LoggerName name) Nothing =
    liftIO $ updateGlobalLogger name clearLevel
setSeverityMaybe n (Just x) = setSeverity n x



-- | Shortcut for `logMessage` to use according severity.
logDebug, logInfo, logNotice, logWarning, logError
    :: (WithNamedLogger m, MonadIO m)
    => T.Text -> m ()
logDebug   = logMessage Debug
logInfo    = logMessage Info
logNotice  = logMessage Notice
logWarning = logMessage Warning
logError   = logMessage Error

-- | Logs message with specified severity using logger name in context.
logMessage
    :: (WithNamedLogger m, MonadIO m)
    => Severity -> T.Text -> m ()
logMessage severity t = do
    LoggerName{..} <- getLoggerName
    liftIO . logM loggerName (convertSeverity severity) $ T.unpack t
