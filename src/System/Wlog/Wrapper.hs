{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}

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
       , initTerminalLogging
       , releaseAllHandlers
       , setSeverity
       , setSeverityMaybe
       ) where

import           Universum

import           Control.Concurrent.MVar    (withMVar)
import           System.IO                  (Handle, stderr, stdout)

import           System.Wlog.Formatter      (stderrFormatter, stdoutFormatter)
import           System.Wlog.Handler        (LogHandler (setFormatter))
import           System.Wlog.Handler.Simple (GenericHandler (..), streamHandler)
import           System.Wlog.Logger         (clearLevel, removeAllHandlers,
                                             rootLoggerName, setHandlers, setLevel,
                                             updateGlobalLogger)
import           System.Wlog.LoggerName     (LoggerName (..))
import           System.Wlog.Severity       (Severity (..))

-- | Like `streamHandler`, but syncronized using given `MVar` as lock
-- (it should be filled before this function call).
streamHandlerWithLock :: MVar () -> Handle -> Severity -> IO (GenericHandler Handle)
streamHandlerWithLock lock h sev = do
    GenericHandler{..} <- streamHandler h sev
    return GenericHandler
        { severity  = severity
        , formatter = formatter
        , privData  = privData
        , writeFunc = \a s -> withMVar lock $ const $ writeFunc a s
        , closeFunc = closeFunc
        , ghTag = ghTag
        }

-- | This function initializes global logging system for terminal output.
-- At high level, it sets severity which will be used by all loggers by default,
-- sets default formatters and sets custom severity for given loggers (if any).
--
-- NOTE: you probably don't want to use this function.
-- Consider 'System.Wlog.Launcher.setupLogging'.
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
initTerminalLogging :: MonadIO m => Bool -> Maybe Severity -> m ()
initTerminalLogging isShowTime (fromMaybe Warning -> defaultSeverity) = liftIO $ do
    lock <- liftIO $ newMVar ()
    -- We set Debug here, to allow all messages by stdout handler.
    -- They will be filtered by loggers.
    stdoutHandler <- setStdoutFormatter <$>
        streamHandlerWithLock lock stdout Debug
    stderrHandler <- setStderrFormatter <$>
        streamHandlerWithLock lock stderr Error
    updateGlobalLogger rootLoggerName $
        setHandlers [stderrHandler, stdoutHandler]
    updateGlobalLogger rootLoggerName $
        setLevel defaultSeverity
  where
    setStdoutFormatter = (`setFormatter` stdoutFormatter isShowTime)
    setStderrFormatter = (`setFormatter` stderrFormatter)

-- | Set severity for given logger. By default parent's severity is used.
setSeverity :: MonadIO m => LoggerName -> Severity -> m ()
setSeverity (LoggerName name) =
    liftIO . updateGlobalLogger name . setLevel

-- | Set or clear severity.
setSeverityMaybe
    :: MonadIO m
    => LoggerName -> Maybe Severity -> m ()
setSeverityMaybe (LoggerName name) Nothing =
    liftIO $ updateGlobalLogger name clearLevel
setSeverityMaybe n (Just x) = setSeverity n x

-- | Lifted alias to 'removeAllHandlers'.
releaseAllHandlers :: MonadIO m => m ()
releaseAllHandlers = liftIO removeAllHandlers
