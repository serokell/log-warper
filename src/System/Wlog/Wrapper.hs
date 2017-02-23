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
       , convertSeverity
       , initTerminalLogging
       , releaseAllHandlers
       , setSeverity
       , setSeverityMaybe
       ) where

import           Universum

import           Control.Concurrent.MVar   (MVar, newMVar, withMVar)
import           System.IO                 (Handle, stderr, stdout)
import           System.Log.Handler.Simple (GenericHandler (..), streamHandler)
import           System.Log.Logger         (Priority (DEBUG, ERROR), clearLevel,
                                            removeAllHandlers, rootLoggerName,
                                            setHandlers, setLevel, updateGlobalLogger)

import           System.Wlog.Formatter     (setStderrFormatter, setStdoutFormatter)
import           System.Wlog.LoggerName    (LoggerName (..))
import           System.Wlog.Severity      (Severity (..), convertSeverity)

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
    -- We set DEBUG here, to allow all messages by stdout handler.
    -- They will be filtered by loggers.
    stdoutHandler <- setStdoutFormatter isShowTime <$>
        streamHandlerWithLock lock stdout DEBUG
    stderrHandler <- setStderrFormatter <$>
        streamHandlerWithLock lock stderr ERROR
    updateGlobalLogger rootLoggerName $
        setHandlers [stderrHandler, stdoutHandler]
    updateGlobalLogger rootLoggerName $
        setLevel (convertSeverity defaultSeverity)

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

-- | Lifted alias to 'removeAllHandlers'.
releaseAllHandlers :: MonadIO m => m ()
releaseAllHandlers = liftIO removeAllHandlers
