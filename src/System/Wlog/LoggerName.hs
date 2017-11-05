{-# LANGUAGE TemplateHaskell #-}

-- | Contains @newtype@ wrapper around logger name to support hierarchy.

module System.Wlog.LoggerName
       ( LoggerName (..)
       , loggerNameF
       ) where

import           Universum

import           Data.Hashable       (Hashable)
import           Data.SafeCopy       (base, deriveSafeCopySimple)
import           Data.Semigroup      (Semigroup)
import qualified Data.Semigroup      as Semigroup
import           Data.String         (IsString)
import           Data.Text.Buildable (Buildable)
import qualified Data.Text.Buildable as Buildable
import           Formatting          (Format, bprint, build, stext)

-- | Logger name to keep in context.
newtype LoggerName = LoggerName
    { getLoggerName :: Text
    } deriving (Show, IsString, Eq, Ord, Hashable)

deriveSafeCopySimple 0 'base ''LoggerName

-- | Defined such that @n1@ is parent for @(n1 <> n2)@
-- (see <http://hackage.haskell.org/package/hslogger-1.2.10/docs/System-Log-Logger.html hslogger description>).
instance Semigroup LoggerName where
    LoggerName parent <> LoggerName suffix
        | null parent = LoggerName suffix
        | null suffix = LoggerName parent
        | otherwise   = LoggerName $ parent <> "." <> suffix

instance Monoid LoggerName where
    mempty = ""
    mappend = (Semigroup.<>)

instance Buildable LoggerName where
    build = bprint stext . getLoggerName

-- | 'LoggerName' formatter which restricts type.
loggerNameF :: Format r (LoggerName -> r)
loggerNameF = build
