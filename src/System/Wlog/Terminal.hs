{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}

-- |
-- Module      : System.Wlog.Terminal
-- Copyright   : (c) Serokell, 2016
-- License     : GPL-3 (see the file LICENSE)
-- Maintainer  : Serokell <hi@serokell.io>
-- Stability   : experimental
-- Portability : POSIX, GHC
--
-- Logging functionality. This module is wrapper over
-- <http://hackage.haskell.org/package/hslogger hslogger>,
-- which allows to keep logger name in monadic context.

module System.Wlog.Terminal
       ( initTerminalLogging
       ) where

import           Universum

import           Data.Time                     (UTCTime)
import           System.IO                     (Handle, stderr, stdout)

import           System.Wlog.Formatter         (stderrFormatter, stdoutFormatter)
import           System.Wlog.IOLogger          (rootLoggerName, setHandlers, setLevel,
                                                updateGlobalLogger)
import           System.Wlog.LogHandler        (LogHandler (setFormatter))
import           System.Wlog.LogHandler.Simple (streamHandler)
import           System.Wlog.Severity          (Severities, errorPlus, excludeError,
                                                warningPlusWoError)

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
initTerminalLogging :: MonadIO m
                    => (UTCTime -> Text)
                    -> (Handle -> Text -> IO ())
                    -> Bool  -- ^ Show time?
                    -> Bool  -- ^ Show ThreadId?
                    -> Maybe Severities
                    -> m ()
initTerminalLogging
    timeF
    customConsoleAction
    isShowTime
    isShowTid
    (fromMaybe (warningPlusWoError) -> defaultSeverity)
  = liftIO $ do
    lock <- liftIO $ newMVar ()
    stdoutHandler <- setStdoutFormatter <$>
        streamHandler stdout customConsoleAction lock (excludeError defaultSeverity)
    stderrHandler <- setStderrFormatter <$>
        streamHandler stderr customConsoleAction lock errorPlus
    updateGlobalLogger rootLoggerName $
        setHandlers [stderrHandler, stdoutHandler]
    updateGlobalLogger rootLoggerName $
        setLevel defaultSeverity
  where
    setStdoutFormatter = (`setFormatter` stdoutFormatter timeF isShowTime isShowTid)
    setStderrFormatter = (`setFormatter` stderrFormatter timeF isShowTid)
