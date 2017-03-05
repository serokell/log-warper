{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}

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
       ( formatLogMessage
       , formatLogMessageColors
       , stdoutFormatter
       , stderrFormatter
       , stdoutFormatterTimeRounded
       , getRoundedTime

       -- * Taken from @hslogger@.
       , LogFormatter
       , nullFormatter
       , simpleLogFormatter
       , tfLogFormatter
       , varFormatter
       ) where

import           Data.List              (span)
import           Data.Monoid            (mconcat)
import           Data.String            (IsString)
import qualified Data.Text              as T
import           Data.Time              (formatTime, getCurrentTime, getZonedTime)
import           Data.Time.Clock        (UTCTime (..))
import           Data.Time.Format       (FormatTime)
import           Formatting             (Format, sformat, shown, stext, (%))
import           Universum
#ifndef mingw32_HOST_OS
import           System.Posix.Process   (getProcessID)
#endif
#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format       (defaultTimeLocale)
#else
import           System.Locale          (defaultTimeLocale)
#endif

import           System.Wlog.Color      (colorizer, colorizerT)
import           System.Wlog.LoggerName (LoggerName, loggerNameF)
import           System.Wlog.Severity   (LogRecord, Severity (..))


----------------------------------------------------------------------------
-- Basic formatting functionality (initially taken from hslogger)
----------------------------------------------------------------------------

-- | A LogFormatter is used to format log messages.  Note that it is
-- paramterized on the 'Handler' to allow the formatter to use
-- information specific to the handler (an example of can be seen in
-- the formatter used in 'System.Log.Handler.Syslog')
type LogFormatter a
    =  a         -- ^ The LogHandler that the passed message came from
    -> LogRecord -- ^ The log message and priority
    -> String    -- ^ The logger name
    -> IO Text   -- ^ The formatted log message

-- | Returns the passed message as is, ie. no formatting is done.
nullFormatter :: LogFormatter a
nullFormatter _ (_,msg) _ = pure msg

-- | Replace some '$' variables in a string with supplied values
replaceVarM
    :: [(String, IO Text)] -- ^ A list of (variableName, action to
                           -- get the replacement string) pairs
    -> String              -- ^ String to perform substitution on
    -> IO Text             -- ^ Resulting string
replaceVarM _ [] = pure ""
replaceVarM keyVals (span (/= '$') -> (before,after)) = do
    (f, rest) <- replaceStart keyVals $ drop 1 after
    if null rest then pure $ T.pack before
    else do
        repRest <- replaceVarM keyVals rest
        pure $ T.pack before <> f <> repRest
  where
    replaceStart [] str = return ("$", str)
    replaceStart ((k, v):kvs) str
        | k `isPrefixOf` str = do
            vs <- v
            return (vs, drop (length k) str)
        | otherwise = replaceStart kvs str

-- | An extensible formatter that allows new substition /variables/ to
-- be defined.  Each variable has an associated IO action that is used
-- to produce the string to substitute for the variable name.  The
-- predefined variables are the same as for 'simpleLogFormatter'
-- /excluding/ @$time@ and @$utcTime@.
varFormatter :: [(String, IO Text)] -> String -> LogFormatter a
varFormatter vars format _h (prio,msg) loggername = do
    replaceVarM (vars ++ predefinedVars) format
  where
    predefinedVars = [ ("msg", pure msg)
                     , ("prio", pure $ show prio)
                     , ("loggername", pure $ T.pack loggername)
                     , ("tid", show <$> myThreadId)
#ifndef mingw32_HOST_OS
                     , ("pid", show <$> getProcessID)
#endif
                     ]


-- | Like 'simpleLogFormatter' but allow the time format to be
-- specified in the first parameter (this is passed to
-- 'Date.Time.Format.formatTime')
tfLogFormatter :: String -> String -> LogFormatter a
tfLogFormatter timeFormat format = do
    let ftime :: FormatTime t => t -> Text
        ftime = T.pack . formatTime defaultTimeLocale timeFormat
    varFormatter [ ("time", ftime <$> getZonedTime)
                 , ("utcTime", ftime <$> getCurrentTime)
                 ]
        format

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
simpleLogFormatter :: String -> LogFormatter a
simpleLogFormatter format h (prio, msg) loggername =
    tfLogFormatter "%F %X %Z" format h (prio,msg) loggername

----------------------------------------------------------------------------
-- Log-warper functionality
----------------------------------------------------------------------------

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
        mconcat [colorizer Error "[$loggername:$prio:$tid] ", timeFmt, "$msg"]

stdoutFmt :: Severity -> Bool -> String
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

-- TODO: do we need coloring here?
formatLogMessage :: LoggerName -> Severity -> UTCTime -> Text -> Text
formatLogMessage = sformat ("["%loggerNameF%":"%shown%"] ["%utcTimeF%"] "%stext)
  where
    utcTimeF :: Format r (UTCTime -> r)
    utcTimeF = shown

-- | Same as 'formatLogMessage', but with colorful output
formatLogMessageColors :: LoggerName -> Severity -> UTCTime -> Text -> Text
formatLogMessageColors lname severity time msg =
    colorizerT severity prefix <> " " <> msg
  where
    prefix = sformat ("["%loggerNameF%":"%shown%"] ["%utcTimeF%"]") lname severity time
    utcTimeF :: Format r (UTCTime -> r)
    utcTimeF = shown
