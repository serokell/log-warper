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

import           Data.Time                  (UTCTime)
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
streamHandlerWithLock :: (Handle -> Text -> IO ())
                      -> MVar ()
                      -> Handle
                      -> (Handle -> Bool)
                      -> Severity
                      -> IO (GenericHandler Handle)
streamHandlerWithLock customTerminalAction lock handle shouldPrintError severity =
    streamHandler handle customTerminalAction shouldPrintError lock severity

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
                    -> Maybe Severity
                    -> m ()
initTerminalLogging
    timeF
    customConsoleAction
    isShowTime
    isShowTid
    (fromMaybe Warning -> defaultSeverity)
  = liftIO $ do
    lock <- liftIO $ newMVar ()
    stdoutHandler <- setStdoutFormatter <$>
        streamHandlerWithLock customConsoleAction lock stdout (const False) defaultSeverity
    stderrHandler <- setStderrFormatter <$>
        streamHandlerWithLock customConsoleAction lock stderr (const True) Error
    updateGlobalLogger rootLoggerName $
        setHandlers [stderrHandler, stdoutHandler]
    updateGlobalLogger rootLoggerName $
        setLevel defaultSeverity
  where
    setStdoutFormatter = (`setFormatter` stdoutFormatter timeF isShowTime isShowTid)
    setStderrFormatter = (`setFormatter` stderrFormatter timeF isShowTid)

-- | Set severity for given logger. By default parent's severity is used.
setSeverity :: MonadIO m => LoggerName -> Severity -> m ()
setSeverity name =
    liftIO . updateGlobalLogger name . setLevel

-- | Set or clear severity.
setSeverityMaybe
    :: MonadIO m
    => LoggerName -> Maybe Severity -> m ()
setSeverityMaybe name Nothing =
    liftIO $ updateGlobalLogger name clearLevel
setSeverityMaybe n (Just x) = setSeverity n x

-- | Lifted alias to 'removeAllHandlers'.
releaseAllHandlers :: MonadIO m => m ()
releaseAllHandlers = liftIO removeAllHandlers
