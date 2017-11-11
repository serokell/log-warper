{-# LANGUAGE NoImplicitPrelude #-}

-- | Custom wrapper around @hslogger.Priority@.

module System.Wlog.Severity
       ( Severity (..)
       , LogRecord(..)
       ) where

import           Universum

import           Data.Typeable (Typeable)
import           Data.Yaml     (FromJSON, ToJSON)
import           GHC.Generics  (Generic)

-- | Severity is level of log message importance. It uniquely
-- determines which messages to print.
data Severity
    = Debug        -- ^ Debug messages
    | Info         -- ^ Information
    | Notice       -- ^ Important (more than average) information
    | Warning      -- ^ General warnings
    | Error        -- ^ General errors/severe errors
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic, Typeable)

instance FromJSON Severity
instance ToJSON   Severity

-- | Internal type of log records.
data LogRecord = LR !Severity !Text deriving Show
