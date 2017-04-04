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
       ( module Export
       ) where

import           System.Wlog.CanLog         as Export
import           System.Wlog.FileUtils      as Export
import           System.Wlog.Handler.Roller as Export
import           System.Wlog.Handler.Simple as Export
import           System.Wlog.Handler.Syslog as Export
import           System.Wlog.Launcher       as Export
import           System.Wlog.Logger         as Export
import           System.Wlog.LoggerConfig   as Export
import           System.Wlog.LoggerName     as Export
import           System.Wlog.LoggerNameBox  as Export
import           System.Wlog.Severity       as Export
import           System.Wlog.Wrapper        as Export
