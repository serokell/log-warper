-- | Level of logged message severity.

module Logw.Severity
       ( -- * Types
         Severity (..)
       , Severities

         -- * Severity utilities
       , allSeverities
       , severityPlus
       , debugPlus, infoPlus
       , noticePlus
       , warningPlus, errorPlus
       , excludeError

         -- * JSON
       , severitiesJsonP
       ) where

import Universum

import Data.Aeson (FromJSON (parseJSON), Object, ToJSON, Value (..))
import Data.Aeson.Types (Parser)
import Fmt (build)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Vector as Vector

{- | Severity is level of log message importance. It uniquely determines which
messages to print.
-}
data Severity
    = Debug        -- ^ Debug messages
    | Info         -- ^ Information
    | Notice       -- ^ Important (more than average) information
    | Warning      -- ^ General warnings
    | Error        -- ^ General errors/severe errors
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

instance FromJSON Severity
instance ToJSON   Severity

instance Buildable Severity where
    build = \case
        Debug   -> "DEBUG"
        Info    -> "INFO"
        Notice  -> "NOTICE"
        Warning -> "WARNING"
        Error   -> "ERROR"

-- | Set of 'Severity'.
type Severities = Set Severity

----------------------------------------------------------------------------
-- Helpers for creating Severities.
----------------------------------------------------------------------------

-- | 'Set' of all 'Severity's.
allSeverities :: Set Severity
allSeverities = Set.fromAscList [minBound .. maxBound]

-- | Returns the 'Set' of 'Severity's of elements greater or equal to the given value.
severityPlus :: Severity -> Set Severity
severityPlus s = Set.fromAscList [s .. maxBound]

-- | Returns 'Set' of 'Severity's not less than 'Debug'.
debugPlus :: Set Severity
debugPlus = severityPlus Debug

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

----------------------------------------------------------------------------
-- JSON utitlities
----------------------------------------------------------------------------

{- | Smart JSON parser for 'Severities'. To specify 'Severities' you can either
use one of key words or manually specify array of 'Severity'. If you want empty
'Set' of 'Severity' just don't specify such field.

Supported keywords:

1. @Error+@: uses 'errorPlus'.
2. @Warning+@: uses 'warningPlus'.
3. @Notice+@: uses 'noticePlus'.
4. @Info+@: uses 'infoPlus'.
5. @Debug+@: uses 'debugPlus'.
6. @All@: same as @Debug+@.
-}
severitiesJsonP :: Object -> Text -> Parser Severities
severitiesJsonP object fieldName =
    case HashMap.lookup fieldName object of
        Just value -> case value of
            String word -> case word of
                "All"      -> pure allSeverities
                "Debug+"   -> pure debugPlus
                "Info+"    -> pure infoPlus
                "Notice+"  -> pure noticePlus
                "Warning+" -> pure warningPlus
                "Error+"   -> pure errorPlus
                _          -> fail $ toString $ "Unknown severity keyword: " <> word
            Array sevs  -> Set.fromList . toList <$> Vector.mapM parseJSON sevs
            _           -> fail "Incorrect severities format. Should be string keyword or array of severites."
        Nothing    -> pure mempty
