{-# LANGUAGE TemplateHaskell #-}

-- | Custom wrapper around @hslogger.Priority@.

module System.Wlog.Severity
       ( Severity (..)
       , convertSeverity
       ) where

import           Data.SafeCopy     (base, deriveSafeCopySimple)
import           Data.Typeable     (Typeable)
import           Data.Yaml         (FromJSON, ToJSON)
import           GHC.Generics      (Generic)
import           System.Log.Logger (Priority (DEBUG, ERROR, INFO, NOTICE, WARNING))

-- | This type specifies which messages to print.
data Severity
    = Debug
    | Info
    | Notice
    | Warning
    | Error
    deriving (Generic, Typeable, Show, Read, Eq)

instance FromJSON Severity
instance ToJSON   Severity

deriveSafeCopySimple 0 'base ''Severity

-- | Maps 'Severity' into 'Priority'.
convertSeverity :: Severity -> Priority
convertSeverity Debug   = DEBUG
convertSeverity Info    = INFO
convertSeverity Notice  = NOTICE
convertSeverity Warning = WARNING
convertSeverity Error   = ERROR
