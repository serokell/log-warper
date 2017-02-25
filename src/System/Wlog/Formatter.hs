{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : System.Wlog.Formatter
-- Copyright   : (c) Serokell, 2016
-- License     : GPL-3 (see the file LICENSE)
-- Maintainer  : Serokell <hi@serokell.io>
-- Stability   : experimental
-- Portability : POSIX, GHC
--
-- This module contains pretty looking formatters for logger.

module System.Wlog.Formatter
       ( formatLogMessage
       , formatLogMessageColors
       , setStderrFormatter
       , setStdoutFormatter
       , stdoutFormatterTimeRounded
       , getRoundedTime
       ) where

import           Data.Monoid            (mconcat)
import           Data.String            (IsString)
import           Data.Text              (Text, pack, unpack)
import           Data.Time.Clock        (UTCTime (..), getCurrentTime)
import           Data.Time.Format       (defaultTimeLocale, formatTime)
import           Formatting             (Format, sformat, shown, stext, (%))

import           System.Log.Formatter   (LogFormatter, simpleLogFormatter)
import           System.Log.Handler     (LogHandler (setFormatter))
import           System.Log.Logger      (Priority (ERROR))

import           System.Wlog.Color      (colorizer)
import           System.Wlog.LoggerName (LoggerName, loggerNameF)
import           System.Wlog.Severity   (Severity, convertSeverity)

import           Universum

timeFmt :: IsString s => s
timeFmt = "[$time] "

timeFmtStdout :: IsString s => Bool -> s
timeFmtStdout = bool "" timeFmt

getRoundedTime :: Int -> IO UTCTime
getRoundedTime roundN = do
    UTCTime{..} <- liftIO $ getCurrentTime
    let newSec = fromIntegral $ roundBy (round $ toRational utctDayTime :: Int)
    pure $ UTCTime { utctDayTime = newSec, .. }
  where
    roundBy :: (Num a, Integral a) => a -> a
    roundBy x = let y = x `div` fromIntegral roundN in y * fromIntegral roundN

stderrFormatter :: LogFormatter a
stderrFormatter =
    simpleLogFormatter $
        mconcat [colorizer ERROR "[$loggername:$prio:$tid] ", timeFmt, "$msg"]

stdoutFmt :: Priority -> Bool -> String
stdoutFmt pr isShowTime = mconcat
    [colorizer pr "[$loggername:$prio:$tid] ", timeFmtStdout isShowTime, "$msg"]

stdoutFormatter :: Bool -> LogFormatter a
stdoutFormatter isShowTime handle r@(pr, _) =
    simpleLogFormatter (stdoutFmt pr isShowTime) handle r

stdoutFormatterTimeRounded :: Int -> LogFormatter a
stdoutFormatterTimeRounded roundN a r@(pr,_) s = do
    t <- getRoundedTime roundN
    simpleLogFormatter (fmt t) a r s
  where
    fmt time = mconcat $
        [ colorizer pr "[$loggername:$prio:$tid] ["
        , formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" time
        , "] $msg"]

setStdoutFormatter :: LogHandler h => Bool -> h -> h
setStdoutFormatter isShowTime = (`setFormatter` stdoutFormatter isShowTime)

setStderrFormatter :: LogHandler h => h -> h
setStderrFormatter = (`setFormatter` stderrFormatter)

-- TODO: do we need coloring here?
formatLogMessage :: LoggerName -> Severity -> UTCTime -> Text -> Text
formatLogMessage = sformat ("["%loggerNameF%":"%shown%"] ["%utcTimeF%"] "%stext)
  where
    utcTimeF :: Format r (UTCTime -> r)
    utcTimeF = shown

-- | Same as 'formatLogMessage', but with colorful output
formatLogMessageColors :: LoggerName -> Severity -> UTCTime -> Text -> Text
formatLogMessageColors lname severity time msg =
    pack (colorizer pr $ unpack prefix) <> " " <> msg
  where
    pr = convertSeverity severity
    prefix = sformat ("["%loggerNameF%":"%shown%"] ["%utcTimeF%"]") lname severity time
    utcTimeF :: Format r (UTCTime -> r)
    utcTimeF = shown
