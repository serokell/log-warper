-- |
-- Module      : System.Wlog
-- Copyright   : (c) Serokell, 2016
-- License     : GPL-3 (see the file LICENSE)
-- Maintainer  : Serokell <hi@serokell.io>
-- Stability   : experimental
-- Portability : POSIX, GHC
--
-- Logging functionality. This module is wrapper over
-- <http://hackage.haskell.org/package/hslogger hslogger>,
-- which allows to keep logger name in monadic context.
-- Messages are colored depending on used serverity.

module System.Wlog
       ( module System.Wlog.CanLog
       , module System.Wlog.Concurrent
       , module System.Wlog.Exception
       , module System.Wlog.FileUtils
       , module System.Wlog.HasLoggerName
       , module System.Wlog.IOLogger
       , module System.Wlog.Launcher
       , module System.Wlog.LoggerConfig
       , module System.Wlog.LoggerName
       , module System.Wlog.LoggerNameBox
       , module System.Wlog.LogHandler.Roller
       , module System.Wlog.LogHandler.Simple
       , module System.Wlog.PureLogging
       , module System.Wlog.Severity
       , module System.Wlog.Terminal
       ) where

import System.Wlog.CanLog
import System.Wlog.Concurrent
import System.Wlog.Exception
import System.Wlog.FileUtils
import System.Wlog.HasLoggerName
import System.Wlog.IOLogger
import System.Wlog.Launcher
import System.Wlog.LoggerConfig
import System.Wlog.LoggerName
import System.Wlog.LoggerNameBox
import System.Wlog.LogHandler.Roller
import System.Wlog.LogHandler.Simple
import System.Wlog.PureLogging
import System.Wlog.Severity
import System.Wlog.Terminal
