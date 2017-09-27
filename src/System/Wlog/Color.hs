-- |
-- Module      : System.Wlog.Color
-- Copyright   : (c) Serokell, 2016
-- License     : GPL-3 (see the file LICENSE)
-- Maintainer  : Serokell <hi@serokell.io>
-- Stability   : experimental
-- Portability : POSIX, GHC
--
-- Colorizing utilities for log messages.
-- Messages are colored depending on used serverity.

module System.Wlog.Color
       ( colorizer
       , colorizerBuilder
       ) where

import qualified Data.Text              as T
import           Data.Text.Lazy.Builder as B
import           System.Console.ANSI    (Color (Blue, Green, Magenta, Red, Yellow),
                                         ColorIntensity (Vivid),
                                         ConsoleLayer (Foreground),
                                         SGR (Reset, SetColor), setSGRCode)
import           Universum

import           System.Wlog.Severity   (Severity (..))

-- | Defines pre- and post-printed characters for printing colorized text.
table :: Severity -> (String, String)
table severity = case severity of
    Error   -> (setColor Red     , reset)
    Debug   -> (setColor Green   , reset)
    Notice  -> (setColor Magenta , reset)
    Warning -> (setColor Yellow  , reset)
    Info    -> (setColor Blue    , reset)
  where
    setColor color = setSGRCode [SetColor Foreground Vivid color]
    reset = setSGRCode [Reset]

-- | Colorizes "Text".
colorizer :: Severity -> Text -> Text
colorizer pr s =
    let (before, after) = table pr in T.pack before <> s <> T.pack after

colorizerBuilder :: Severity -> Builder -> Builder
colorizerBuilder pr s =
    let (before, after) = table pr in B.fromText (T.pack before) <> s <> B.fromText (T.pack after)
