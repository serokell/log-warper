{-# LANGUAGE NoImplicitPrelude #-}

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
       , isValidRotation

         -- * Builders for 'LoggerConfig'
       , consoleOutB
       , mapperB
       , prefixB
       , productionB
       , showTimeB
       ) where

import           Universum

import           Data.Aeson             (withObject)
import qualified Data.HashMap.Strict    as HM hiding (HashMap)
import           Data.List              (notElem)
import           Data.Monoid            (Any (..))
import           Data.Text              (Text)
import qualified Data.Text.Buildable    as Buildable
import           Data.Traversable       (for)
import           Data.Word              (Word64)
import           Data.Yaml              (FromJSON (..), ToJSON (..), Value (Object),
                                         object, (.!=), (.:), (.:?), (.=))
import           Formatting             (bprint, shown)
import           GHC.Generics           (Generic)

import           System.Wlog.LoggerName (LoggerName)
import           System.Wlog.Wrapper    (Severity)

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


-- TODO: QuickCheck tests on monoid laws
instance Monoid LoggerTree where
    mempty = LoggerTree
        { ltFile       = Nothing
        , ltSeverity   = Nothing
        , ltSubloggers = mempty
        }

    lt1 `mappend` lt2 = LoggerTree
        { ltFile        = ltFile      lt1 <|> ltFile       lt2
        , ltSeverity   = ltSeverity   lt1 <|> ltSeverity   lt2
        , ltSubloggers = ltSubloggers lt1  <> ltSubloggers lt2
        }

nonLoggers :: [Text]
nonLoggers = ["file", "severity"]

instance ToJSON LoggerTree
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

instance Buildable.Buildable RotationParameters where
    build = bprint shown

instance ToJSON RotationParameters

instance FromJSON RotationParameters where
    parseJSON = withObject "rotation params" $ \o -> do
        rpLogLimit  <- o .: "logLimit"
        rpKeepFiles <- o .: "keepFiles"
        return RotationParameters{..}

-- | Checks if logger rotation parameters are valid.
isValidRotation :: RotationParameters -> Bool
isValidRotation RotationParameters{..} = rpLogLimit > 0 && rpKeepFiles > 0

----------------------------------------------------------------------------
-- LoggerConfig
----------------------------------------------------------------------------

-- | Logger configuration which keeps 'RotationParameters' and 'LoggerTree'.
data LoggerConfig = LoggerConfig
    { -- | Rotation parameters for logger config. See 'System.Wlog.Roller'.
      lcRotation      :: Maybe RotationParameters

      -- | Severity for terminal output. If @Nothing@ then 'Warning' is used.
    , lcTermSeverity  :: Maybe Severity

      -- | Show time for non-error messages.
      -- Note that error messages always have timestamp.
    , lcShowTime      :: Any

      -- | @True@ if we should also print output into console.
    , lcConsoleOutput :: Any

      -- | Defines how to transform logger names in config.
    , lcMapper        :: Endo LoggerName

      -- | Path prefix to add for each logger file
    , lcFilePrefix    :: Maybe FilePath

      -- | Hierarchical tree of loggers.
    , lcTree          :: LoggerTree
    }

-- TODO: QuickCheck tests on monoid laws
instance Monoid LoggerConfig where
    mempty = LoggerConfig
        { lcRotation      = Nothing
        , lcTermSeverity  = Nothing
        , lcShowTime      = mempty
        , lcConsoleOutput = mempty
        , lcMapper        = mempty
        , lcFilePrefix    = mempty
        , lcTree          = mempty
        }

    lc1 `mappend` lc2 = LoggerConfig
        { lcRotation      = lcRotation      lc1 <|> lcRotation      lc2
        , lcTermSeverity  = lcTermSeverity  lc1 <|> lcTermSeverity  lc2
        , lcShowTime      = lcShowTime      lc1  <> lcShowTime      lc2
        , lcConsoleOutput = lcConsoleOutput lc1  <> lcConsoleOutput lc2
        , lcMapper        = lcMapper        lc1  <> lcMapper        lc2
        , lcFilePrefix    = lcFilePrefix    lc1 <|> lcFilePrefix    lc2
        , lcTree          = lcTree          lc1  <> lcTree          lc2
        }

topLevelParams :: [Text]
topLevelParams = ["rotation", "showTime", "printOutput", "filePrefix"]

instance FromJSON LoggerConfig where
    parseJSON = withObject "rotation params" $ \o -> do
        lcRotation      <-         o .:? "rotation"
        lcTermSeverity  <-         o .:? "termSeverity"
        lcShowTime      <- Any <$> o .:? "showTime"    .!= False
        lcConsoleOutput <- Any <$> o .:? "printOutput" .!= False
        lcFilePrefix    <-         o .:? "filePrefix"
        lcTree          <- parseJSON $ Object $ filterObject topLevelParams o
        let lcMapper     = mempty
        return LoggerConfig{..}

-- | This instances violates @fromJSON . toJSON = identity@ rule but doesn't matter
-- because it is used only for debugging.
instance ToJSON LoggerConfig where
    toJSON LoggerConfig{..} = object
            [ "rotation"     .= lcRotation
            , "termSeverity" .= lcTermSeverity
            , "showTime"     .= getAny lcShowTime
            , "printOutput"  .= getAny lcConsoleOutput
            , "filePrefix"   .= lcFilePrefix
            , ("logTree", toJSON lcTree)
            ]

-- Builders for 'LoggerConfig'.

-- | Setup 'lcShowTime' inside 'LoggerConfig'.
showTimeB :: Bool -> LoggerConfig
showTimeB isShowTime = mempty { lcShowTime = Any isShowTime }

-- | Setup 'lcConsoleOutput' inside 'LoggerConfig'.
consoleOutB :: Bool -> LoggerConfig
consoleOutB printToConsole = mempty { lcConsoleOutput = Any printToConsole }

-- | Adds sensible predefined set of parameters to logger.
productionB :: LoggerConfig
productionB = showTimeB True <> consoleOutB True

-- | Setup 'lcMapper' inside 'LoggerConfig'.
mapperB :: (LoggerName -> LoggerName) -> LoggerConfig
mapperB loggerNameMapper = mempty { lcMapper = Endo loggerNameMapper }

-- | Setup 'lcFilePrefix' inside 'LoggerConfig'.
prefixB :: FilePath -> LoggerConfig
prefixB filePrefix = mempty { lcFilePrefix = Just filePrefix }
