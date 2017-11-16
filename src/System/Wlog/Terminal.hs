{-# LANGUAGE ViewPatterns #-}

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

import Universum

import Data.Time (UTCTime)
import System.IO (Handle, stderr, stdout)

import System.Wlog.Formatter (stdoutFormatter)
import System.Wlog.IOLogger (rootLoggerName, setHandlers, setLevel, updateGlobalLogger)
import System.Wlog.LogHandler (LogHandler (setFormatter))
import System.Wlog.LogHandler.Simple (streamHandler)
import System.Wlog.Severity (Severities, debugPlus, errorPlus, excludeError)


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
                    -> Maybe Severities
                    -> m ()
initTerminalLogging
    timeF
    customConsoleAction
    isShowTime
    isShowTid
    maybeSevOut
    maybeSevErr
  = liftIO $ do
    lock <- liftIO $ newMVar ()
    let (severitiesOut, severitiesErr) =
          case (maybeSevOut, maybeSevErr) of
              (Nothing, Nothing)   -> (excludeError debugPlus, errorPlus)
              (Just out, Nothing)  -> (out, mempty)
              (Nothing, Just err)  -> (mempty, err)
              (Just out, Just err) -> (out, err)
    stdoutHandler <- setStdoutFormatter <$>
        streamHandler stdout customConsoleAction lock severitiesOut
    stderrHandler <- setStderrFormatter <$>
        streamHandler stderr customConsoleAction lock severitiesErr
    updateGlobalLogger rootLoggerName $
        setHandlers [stderrHandler, stdoutHandler]
    updateGlobalLogger rootLoggerName $
        setLevel $ severitiesOut <> severitiesErr
  where
    setStdoutFormatter = (`setFormatter` stdoutFormatter timeF isShowTime isShowTid)
    setStderrFormatter = (`setFormatter` stdoutFormatter timeF True isShowTid)
