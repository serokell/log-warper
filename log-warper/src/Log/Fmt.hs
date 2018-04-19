-- | Contains functions to format logging events in 'log-warper'.

module Log.Fmt
       ( -- * Default formatters
         eventF
       , richEventF
       , centiUtcTimeF

         -- * Colors
       , colorTable
       , colorize
       ) where

import Universum

import Data.Time.Clock (UTCTime)
import Fmt (fmt, padRightF, (+|), (|+), (|++|))
import Fmt.Time (dateDashF, hmsF, subsecondF, tzNameF)
import System.Console.ANSI (Color (Blue, Green, Magenta, Red, Yellow), ColorIntensity (Vivid),
                            ConsoleLayer (Foreground), SGR (Reset, SetColor), setSGRCode)

import Log.Event (Event (..), RichEvent (..))
import Log.Severity (Severity (..))

import qualified Data.Text as Text

-- | Defines pre- and post-printed characters for printing colorized text.
colorTable :: Severity -> (String, String)
colorTable = \case
    Error   -> (setColor Red     , reset)
    Warning -> (setColor Yellow  , reset)
    Notice  -> (setColor Magenta , reset)
    Info    -> (setColor Blue    , reset)
    Debug   -> (setColor Green   , reset)
  where
    setColor :: Color -> String
    setColor color = setSGRCode [SetColor Foreground Vivid color]

    reset :: String
    reset = setSGRCode [Reset]

-- | Colorizes 'Text' according to predefined colors for 'Severity'. See
-- 'colorTable' for details.
colorize :: Severity -> Text -> Text
colorize severity message =
    let (before, after) = colorTable severity
    in toText before <> message <> toText after

{- | Colorful formatter for 'Event'. Formats according to the following template:

@
[LoggerName:Severity] Some text message
@
-}
eventF :: Event exts -> Text
eventF Event{..} =
    let header = "[" +| _eName |+ ":" +| _eSeverity |+ "] "
    in colorize _eSeverity header <> _eMessage

{- | Colorful formatter for 'RichEvent'. Formats according to the following template:

@
[LoggerName:Severity:ThreadId] [UTCTime] Some text message
@

@UTCTime@ and @ThreadId@ are printed only if corresponding fields in 'RichEvent'
are not 'Nothing'.
-}
richEventF :: RichEvent exts -> Text
richEventF RichEvent{ _reEvent = Event{..}, ..} =
    let header = "[" +| _eName |+ ":" +| _eSeverity |++| threadIdF |+ "] "
    in colorize _eSeverity header <> timeF <> _eMessage
  where
    threadIdF :: Text
    threadIdF = case _reThread of
        Nothing  -> ""
        Just tid -> ":" +| show @String tid |+ ""

    timeF :: Text
    timeF = case _reTime of
        Nothing   -> ""
        Just time -> "[" +| centiUtcTimeF time |+ "] "

-- | Formats UTC time in next format: @"%Y-%m-%d %H:%M:%S%Q %Z"@
-- but @%Q@ part show only in centiseconds (always 2 digits).
centiUtcTimeF :: UTCTime -> Text
centiUtcTimeF t =
    dateDashF    t |+ " "
 +| hmsF         t |++|
    centiSecondF t |+ " "
 +| tzNameF      t |+ ""
  where
    centiSecondF = padRightF 3 '0' . Text.take 3 . fmt . subsecondF
