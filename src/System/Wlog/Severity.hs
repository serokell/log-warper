{-# LANGUAGE TemplateHaskell #-}

-- | Custom wrapper around @hslogger.Priority@.

module System.Wlog.Severity
       ( Severity (..)
       , LogRecord
       ) where

import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Data.Typeable (Typeable)
import           Data.Yaml     (FromJSON, ToJSON)
import           GHC.Generics  (Generic)
import           Universum

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

deriveSafeCopySimple 0 'base ''Severity

-- | Internal type of log records.
type LogRecord = (Severity, Text)
