-- | Contains @newtype@ wrapper around logger name to support hierarchy.

module System.Wlog.LoggerName
       ( LoggerName (..)
       ) where

import Universum

import Data.Aeson.Types (ToJSON, ToJSONKey (..), toJSONKeyText)
import Fmt (Buildable, build)

-- | Logger name to keep in context.
newtype LoggerName = LoggerName
    { getLoggerName :: Text
    } deriving (Show, IsString, Eq, Ord, Hashable, Generic)

-- | Defined such that @n1@ is parent for @(n1 <> n2)@
-- (see <http://hackage.haskell.org/package/hslogger-1.2.10/docs/System-Log-Logger.html hslogger description>).
instance Semigroup LoggerName where
    LoggerName parent <> LoggerName suffix
        | null parent = LoggerName suffix
        | null suffix = LoggerName parent
        | otherwise   = LoggerName $ parent <> "." <> suffix

instance Monoid LoggerName where
    mempty = ""
    mappend = (<>)

instance Buildable LoggerName where
    build = build . getLoggerName

instance ToJSON LoggerName

instance ToJSONKey LoggerName where
    toJSONKey = toJSONKeyText getLoggerName
