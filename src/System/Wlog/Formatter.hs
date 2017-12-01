{-# LANGUAGE CPP          #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : System.Wlog.Formatter
-- Copyright   : (c) Serokell, 2016
-- License     : GPL-3 (see the file LICENSE)
-- Maintainer  : Serokell <hi@serokell.io>
-- Stability   : experimental
-- Portability : POSIX, GHC
--
-- Pretty looking formatters for logger.
--
-- Please see "System.WLog.Logger" for extensive documentation on the
-- logging system.
module System.Wlog.Formatter
       ( stdoutFormatter
       , stdoutFormatterTimeRounded
       , centiUtcTimeF
       , getRoundedTime

       -- * Taken from @hslogger@.
       , LogFormatter
       , nullFormatter
       , simpleLogFormatter
       , tfLogFormatter
       , varFormatter
       ) where

import Universum

import Control.Concurrent (myThreadId)
import Data.Monoid (mconcat)
import Data.Text.Lazy.Builder as B
import Data.Time (formatTime, getCurrentTime, getZonedTime)
import Data.Time.Clock (UTCTime (..), diffTimeToPicoseconds, picosecondsToDiffTime)
import Data.Time.Format (FormatTime)
import Fmt (fmt, padRightF, (+|), (|+), (|++|))
import Fmt.Time (dateDashF, hmsF, subsecondF, tzNameF)

#ifndef mingw32_HOST_OS
import System.Posix.Process (getProcessID)
#endif
#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

import System.Wlog.Color (colorizer)
import System.Wlog.Severity (LogRecord (..))

import qualified Data.Text as T

----------------------------------------------------------------------------
-- Basic formatting functionality (initially taken from hslogger)
----------------------------------------------------------------------------

-- | A LogFormatter is used to format log messages.  Note that it is
-- paramterized on the 'Handler' to allow the formatter to use
-- information specific to the handler (an example of can be seen in
-- the formatter used in 'System.Log.Handler.Syslog')
type LogFormatter a
    =  a          -- ^ The LogHandler that the passed message came from
    -> LogRecord  -- ^ The log message and priority
    -> Text       -- ^ The logger name
    -> IO Builder -- ^ The formatted log message

-- | Returns the passed message as is, ie. no formatting is done.
nullFormatter :: LogFormatter a
nullFormatter _ (LR _ msg) _ = pure (B.fromText msg)

-- | Replace some '$' variables in a string with supplied values
replaceVars
    :: [(Text, Text)] -- ^ A list of (variableName, action to
                      -- get the replacement string) pairs
    -> Text           -- ^ Text to perform substitution on
    -> Builder        -- ^ Resulting string
replaceVars _ (T.null -> True) = mempty
replaceVars keyVals (T.breakOn "$" -> (before,after)) =
    if T.null after then B.fromText before
    else
        let (f, rest) = replaceStart keyVals $ T.drop 1 after
            repRest   = replaceVars keyVals rest
        in B.fromText before <> f <> repRest
  where
    replaceStart :: [(Text, Text)] -> Text -> (Builder, Text)
    replaceStart [] str = (B.singleton '$', str)
    replaceStart ((k, v):kvs) txt
        | k `T.isPrefixOf` txt = (B.fromText v, T.drop (T.length k) txt)
        | otherwise = replaceStart kvs txt

-- | An extensible formatter that allows new substition /variables/ to
-- be defined.  Each variable has an associated IO action that is used
-- to produce the string to substitute for the variable name.  The
-- predefined variables are the same as for 'simpleLogFormatter'
-- /excluding/ @$time@ and @$utcTime@.
varFormatter :: [(Text, Text)] -> Text -> LogFormatter a
varFormatter vars format _h (LR prio msg) loggername = do
    defaultVars  <- predefinedVars
    platformVars <- osSpecificVars
    return $ replaceVars (vars <> defaultVars <> platformVars) format
  where
    predefinedVars = do
        tid <- T.pack . show <$> myThreadId
        pure [ ("msg", msg)
             , ("prio", T.toUpper $ show prio)
             , ("loggername", loggername)
             , ("tid", tid)
             ]
#ifndef mingw32_HOST_OS
    osSpecificVars = do
      pid <- T.pack . show <$> getProcessID
      pure [("pid", pid)]
#else
    osSpecificVars = return mempty
#endif


-- | Like 'simpleLogFormatter' but allow the time format to be
-- specified in the first parameter (this is passed to
-- 'Date.Time.Format.formatTime')
tfLogFormatter :: Text -> Text -> LogFormatter a
tfLogFormatter timeFormat format = \h kv loggername -> do
    time    <- ftime <$> getZonedTime
    utcTime <- ftime <$> getCurrentTime
    varFormatter [ ("time", time)
                 , ("utcTime", utcTime)
                 ]
        format h kv loggername
  where
     ftime :: FormatTime t => t -> Text
     ftime = T.pack . formatTime defaultTimeLocale (T.unpack timeFormat)

-- | Takes a format string, and returns a formatter that may be used
--   to format log messages.  The format string may contain variables
--   prefixed with a $-sign which will be replaced at runtime with
--   corresponding values.  The currently supported variables are:
--
--    * @$msg@ - The actual log message
--
--    * @$loggername@ - The name of the logger
--
--    * @$prio@ - The priority level of the message
--
--    * @$tid@  - The thread ID
--
--    * @$pid@  - Process ID  (Not available on windows)
--
--    * @$time@ - The current time
--
--    * @$utcTime@ - The current time in UTC Time
simpleLogFormatter :: Text -> LogFormatter a
simpleLogFormatter format h logRecord loggername =
    tfLogFormatter "%F %X %Z" format h logRecord loggername

----------------------------------------------------------------------------
-- Log-warper functionality
----------------------------------------------------------------------------

-- | Formats UTC time in next format: "%Y-%m-%d %H:%M:%S%Q %Z"
-- but %Q part show only in centiseconds (always 2 digits).
centiUtcTimeF :: UTCTime -> Text
centiUtcTimeF t =
    dateDashF    t |+ " "
 +| hmsF         t |++|
    centiSecondF t |+ " "
 +| tzNameF      t |+ ""
  where
    centiSecondF = padRightF 3 '0' . T.take 3 . fmt . subsecondF

-- | Get the current time rounded to 10^n picoseconds.
-- n = 3 rounds to the nearest nanosecond.
-- n = 6 rounds to the nearest microsecond.
-- n = 9 will round to the nearest millisecond.
-- n = 12 will round to the nearest second.
getRoundedTime :: Int -> IO UTCTime
getRoundedTime n = do
    UTCTime{..} <- getCurrentTime
    let m = if n < 0 then 0 else (fromIntegral n :: Integer)
        multiplier = 10 ^ m
        picoseconds = diffTimeToPicoseconds utctDayTime
        roundedPicoseconds = (picoseconds `div` multiplier) * multiplier
    pure $ UTCTime { utctDayTime = picosecondsToDiffTime roundedPicoseconds, .. }

stdoutFormatter :: (UTCTime -> Text) -> Bool -> Bool -> LogFormatter a
stdoutFormatter timeF isShowTime isShowTid handle record message = do
    time <- getCurrentTime
    createLogFormatter isShowTime isShowTid timeF time handle record message

stdoutFormatterTimeRounded :: (UTCTime -> Text) -> Int -> LogFormatter a
stdoutFormatterTimeRounded timeF n handle record message = do
    time <- getRoundedTime n
    createLogFormatter True True timeF time handle record message

createLogFormatter
    :: Bool
    -> Bool
    -> (UTCTime -> Text)
    -> UTCTime
    -> LogFormatter a
createLogFormatter
    isShowTime
    isShowTid
    timeF
    time
    handle
    record@(LR priority _)
  =
    simpleLogFormatter format handle record
  where
    format = mconcat $!
        [ colorizer priority $ "[$loggername:$prio" <> tidShower <> "] "
        , timeShower
        , "$msg"
        ]

    timeShower, tidShower :: Text
    timeShower = if isShowTime then "[" <> timeF time <> "] " else mempty
    tidShower  = if isShowTid  then ":$tid"                   else mempty

{- TODO: not used anymore, but probably should
formatLogMessage :: LoggerName -> Severity -> UTCTime -> Text -> Text
formatLogMessage = sformat ("["%loggerNameF%":"%shown%"] ["%utcTimeF%"] "%stext)
  where
    utcTimeF :: Format r (UTCTime -> r)
    utcTimeF = shown

-- | Same as 'formatLogMessage', but with colorful output
formatLogMessageColors :: LoggerName -> Severity -> UTCTime -> Text -> Text
formatLogMessageColors lname severity time msg =
    colorizer severity prefix <> " " <> msg
  where
    prefix = sformat ("["%loggerNameF%":"%shown%"] ["%utcTimeF%"]") lname severity time
    utcTimeF :: Format r (UTCTime -> r)
    utcTimeF = shown
-}
