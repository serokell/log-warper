{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

{- |
   Module     : System.Log.LogHandler
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Definition of log handler support

For some handlers, check out "System.WLog.Handler.Simple" and
"System.WLog.Handler.Syslog".

Please see "System.WLog.Logger" for extensive documentation on the
logging system.

Written by John Goerzen, jgoerzen\@complete.org
-}
module System.Wlog.LogHandler
       ( -- * Basic Types
         LogHandlerTag(..)
       , LogHandler(..)
       , logHandlerMessage
       ) where

import Universum

import Data.Text.Lazy.Builder as B

import System.Wlog.Formatter (LogFormatter, nullFormatter)
import System.Wlog.LoggerName (LoggerName (..))
import System.Wlog.Severity (LogRecord (..), Severities)

import qualified Data.Set as Set


-- | Logs an event if it meets the requirements
-- given by the most recent call to 'setLevel'.
logHandlerMessage :: (MonadIO m, LogHandler a) => a -> LogRecord -> LoggerName -> m ()
logHandlerMessage h lr@(LR pri _) logname =
    when (pri `Set.member` (getLevel h)) $ do
        let lName = getLoggerName logname
        formattedMsg <- liftIO $ (getFormatter h) h lr lName
        emit h formattedMsg lName

-- | Tag identifying handlers.
data LogHandlerTag
    = HandlerFilelike FilePath
    | HandlerOther String
    deriving (Show, Eq)

-- | This is the base class for the various log handlers.  They should
-- all adhere to this class.
class LogHandler a where
    -- TODO it should be data/type family like in 'System.Exception'
    -- | Tag of handler. Is arbitrary. Made for identification.
    getTag :: a -> LogHandlerTag

    -- | Sets the log level. 'handle' will drop items beneath this
    -- level.
    setLevel :: a -> Severities -> a

    -- | Gets the current level.
    getLevel :: a -> Severities

    -- | Set a log formatter to customize the log format for this Handler
    setFormatter :: a -> LogFormatter a -> a
    getFormatter :: a -> LogFormatter a
    getFormatter _h = nullFormatter

    -- | Forces an event to be logged regardless of
    -- the configured level.
    emit :: MonadIO m => a -> B.Builder -> Text -> m ()

    -- | Read back from logger (e.g. file), newest first. You specify
    -- the number of (newest) logging entries. Logger can return @pure
    -- []@ if this behaviour can't be emulated or store buffer.
    readBack :: MonadIO m => a -> Int -> m [Text]

    -- | Closes the logging system, causing it to close
    -- any open files, etc.
    close :: MonadIO m => a -> m ()
