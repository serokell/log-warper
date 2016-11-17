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
       , LoggerMap
       ) where

import           Data.Aeson          (withObject)
import           Data.Default        (Default (def))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM hiding (HashMap)
import           Data.Text           (Text)
import           Data.Traversable    (for)
import           Data.Yaml           (FromJSON (..), ToJSON, (.:?))
import           GHC.Generics        (Generic)

import           System.Wlog.Wrapper (Severity)

type LoggerMap = HashMap Text LoggerConfig

data LoggerConfig = LoggerConfig
    { lcSubloggers :: LoggerMap
    , lcFile       :: Maybe FilePath
    , lcSeverity   :: Maybe Severity
    } deriving (Generic, Show)

instance ToJSON LoggerConfig

instance Default LoggerConfig where
    def = LoggerConfig
          { lcFile       = Nothing
          , lcSeverity   = Nothing
          , lcSubloggers = mempty
          }

nonLoggers :: [Text]
nonLoggers = ["file", "severity"]

selectLoggers :: HashMap Text a -> HashMap Text a
selectLoggers = HM.filterWithKey $ \k _ -> k `notElem` nonLoggers

instance FromJSON LoggerConfig where
    parseJSON = withObject "root loggers" $ \o -> do
        lcFile       <- o .:? "file"
        lcSeverity   <- o .:? "severity"
        lcSubloggers <- for (selectLoggers o) parseJSON
        return LoggerConfig{..}
