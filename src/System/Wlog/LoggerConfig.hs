-- |
-- Module      : System.Wlog.LoggerConfig
-- Copyright   : (c) Serokell, 2016
-- License     : GPL-3 (see the file LICENSE)
-- Maintainer  : Serokell <hi@serokell.io>
-- Stability   : experimental
-- Portability : POSIX, GHC
--
-- Logger configuration.

module System.Wlog.LoggerConfig
       ( LoggerConfig (..)
       , LoggerTree   (..)
       , LoggerMap
       , RotationParameters (..)
       ) where

import           Data.Aeson          (withObject)
import           Data.Default        (Default (def))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM hiding (HashMap)
import           Data.Text           (Text)
import           Data.Traversable    (for)
import           Data.Word           (Word64)
import           Data.Yaml           (FromJSON (..), ToJSON, Value (Object), (.:), (.:?))
import           GHC.Generics        (Generic)

import           System.Wlog.Wrapper (Severity)

----------------------------------------------------------------------------
-- Utilites & helpers
----------------------------------------------------------------------------

filterObject :: [Text] -> HashMap Text a -> HashMap Text a
filterObject excluded = HM.filterWithKey $ \k _ -> k `notElem` excluded

----------------------------------------------------------------------------
-- LoggerTree
----------------------------------------------------------------------------

type LoggerMap = HashMap Text LoggerTree

-- | Stores configuration for hierarchical loggers.
data LoggerTree = LoggerTree
    { ltSubloggers :: !LoggerMap
    , ltFile       :: !(Maybe FilePath)
    , ltSeverity   :: !(Maybe Severity)
    } deriving (Generic, Show)

instance ToJSON LoggerTree

instance Default LoggerTree where
    def = LoggerTree
          { ltFile       = Nothing
          , ltSeverity   = Nothing
          , ltSubloggers = mempty
          }

nonLoggers :: [Text]
nonLoggers = ["file", "severity"]

instance FromJSON LoggerTree where
    parseJSON = withObject "loggers tree" $ \o -> do
        ltFile       <- o .:? "file"
        ltSeverity   <- o .:? "severity"
        ltSubloggers <- for (filterObject nonLoggers o) parseJSON
        return LoggerTree{..}

----------------------------------------------------------------------------
-- Logger rotattion
----------------------------------------------------------------------------

-- | Parameters for logging rotation.
data RotationParameters = RotationParameters
    { rpLogLimit  :: !Word64  -- ^ max size of file in bytes
    , rpKeepFiles :: !Word    -- ^ number of files to keep
    } deriving (Generic, Show)

instance ToJSON RotationParameters

instance FromJSON RotationParameters where
    parseJSON = withObject "rotation params" $ \o -> do
        rpLogLimit  <- o .: "logLimit"
        rpKeepFiles <- o .: "keepFiles"
        return RotationParameters{..}

----------------------------------------------------------------------------
-- LoggerConfig
----------------------------------------------------------------------------

-- | Logger configuration which keeps 'RotationParameters' and 'LoggerTree'.
data LoggerConfig = LoggerConfig
    { lcRotation :: !(Maybe RotationParameters)
    , lcTree     :: !LoggerTree
    } deriving (Generic, Show)

instance Default LoggerConfig where
    def = LoggerConfig
          { lcRotation = Nothing
          , lcTree     = def
          }

instance FromJSON LoggerConfig where
    parseJSON = withObject "rotation params" $ \o -> do
        lcRotation <- o .:? "rotation"
        lcTree     <- parseJSON $ Object $ filterObject ["rotation"] o
        return LoggerConfig{..}

instance ToJSON LoggerConfig
