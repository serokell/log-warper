{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

{- |
   Module     : System.Log.Handler
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
module System.Wlog.Handler
       ( -- * Basic Types
         LogHandler(..)
       ) where

import           System.Wlog.Formatter (LogFormatter, nullFormatter)
import           System.Wlog.Severity  (LogRecord, Severity)
import           Universum

-- | This is the base class for the various log handlers.  They should
-- all adhere to this class.
class LogHandler a where
    -- TODO it should be data/type family like in 'System.Exception'
    -- | Tag of handler. Is arbitrary. Made for identication.
    getTag :: a -> String

    -- | Sets the log level. 'handle' will drop items beneath this
    -- level.
    setLevel :: a -> Severity -> a

    -- | Gets the current level.
    getLevel :: a -> Severity

    -- | Set a log formatter to customize the log format for this Handler
    setFormatter :: a -> LogFormatter a -> a
    getFormatter :: a -> LogFormatter a
    getFormatter _h = nullFormatter

    -- | Logs an event if it meets the requirements
    -- given by the most recent call to 'setLevel'.
    handle :: a -> LogRecord -> String -> IO ()
    handle h (pri, msg) logname =
        if pri >= (getLevel h)
        then do formattedMsg <- (getFormatter h) h (pri,msg) logname
                emit h (pri, formattedMsg) logname
        else return ()

    -- | Forces an event to be logged regardless of
    -- the configured level.
    emit :: a -> LogRecord -> String -> IO ()

    -- | Closes the logging system, causing it to close
    -- any open files, etc.
    close :: a -> IO ()
