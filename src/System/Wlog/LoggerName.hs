-- | Contains @newtype@ wrapper around logger name to support hierarchy.

module System.Wlog.LoggerName
       ( LoggerName (..)
       ) where

import           Data.Hashable  (Hashable)
import           Data.Semigroup (Semigroup)
import qualified Data.Semigroup as Semigroup
import           Data.String    (IsString)

-- | Logger name to keep in context.
newtype LoggerName = LoggerName
    { loggerName :: String    -- TODO: replace with 'Text'
    } deriving (Show, IsString, Eq, Hashable)

-- | Defined such that @n1@ is parent for @(n1 <> n2)@
-- (see <http://hackage.haskell.org/package/hslogger-1.2.10/docs/System-Log-Logger.html hslogger description>).
instance Semigroup LoggerName where
    LoggerName base <> LoggerName suffix
        | null base   = LoggerName suffix
        | null suffix = LoggerName base
        | otherwise   = LoggerName $ base ++ "." ++ suffix

instance Monoid LoggerName where
    mempty = ""
    mappend = (Semigroup.<>)
