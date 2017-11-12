-- | Custom wrapper around @hslogger.Priority@.

module System.Wlog.Severity
       ( Severity (..)
       , Severities
       , LogRecord(..)
         -- * Severity utilities
       , severityPlus
       , debugPlus, infoPlus
       , noticePlus
       , warningPlus, errorPlus
       , excludeError
       , debugPlusWoError
       , infoPlusWoError
       , noticePlusWoError
       , warningPlusWoError

       ) where

import Universum

import Data.Typeable (Typeable)
import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)

import qualified Data.Set as Set

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

-- | Set of 'Severity'.
type Severities = Set Severity

-- | Internal type of log records.
data LogRecord = LR !Severity !Text deriving Show

-- | 'Set' of all 'Severity's.
allSeverities :: Set Severity
allSeverities = Set.fromList [Debug, Info, Notice, Warning, Error]

-- | Returns the 'Set' of 'Severity's of elements greater than the given value.
severityPlus :: Severity -> Set Severity
severityPlus s = Set.filter (>= s) allSeverities

-- | Returns 'Set' of 'Severity's not less than 'Debug'.
debugPlus :: Set Severity
debugPlus = allSeverities

-- | Returns 'Set' of 'Severity's not less than 'Info'.
infoPlus :: Set Severity
infoPlus = severityPlus Info

-- | Returns 'Set' of 'Severity's not less than 'Notice'.
noticePlus :: Set Severity
noticePlus = severityPlus Notice

-- | Returns 'Set' of 'Severity's not less than 'Warning'.
warningPlus :: Set Severity
warningPlus = severityPlus Warning

-- | Returns 'Set' of 'Severity's not less than 'Error'.
errorPlus :: Set Severity
errorPlus = Set.singleton Error

-- | Excludes 'Error' from the 'Set' of 'Severity's.
excludeError :: Set Severity -> Set Severity
excludeError = Set.delete Error

-- | Similar to 'debugPlus' but excludes 'Error'.
debugPlusWoError :: Set Severity
debugPlusWoError = excludeError allSeverities

-- | Similar to 'infoPlus' but excludes 'Error'.
infoPlusWoError :: Set Severity
infoPlusWoError = excludeError infoPlus

-- | Similar to 'noticePlus' but excludes 'Error'.
noticePlusWoError :: Set Severity
noticePlusWoError = excludeError noticePlus

-- | Similar to 'warningPlus' but excludes 'Error'.
warningPlusWoError :: Set Severity
warningPlusWoError = Set.singleton Warning
