{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

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
       ( LoggerMap
       , RotationParameters (..)
       , fromScratch
       , isValidRotation

         -- * Hierarchical tree of loggers (with lenses)
       , HandlerWrap (..)
       , hwFilePath
       , hwRounding
       , LoggerTree (..)
       , ltFiles
       , ltSeverity
       , ltSubloggers

         -- * Global logger configuration
       , LoggerConfig (..)

         -- ** Lenses
       , lcConsoleOutput
       , lcFilePrefix
       , lcMapper
       , lcRotation
       , lcShowTime
       , lcShowTid
       , lcTermSeverity
       , lcTree
       , zoomLogger

         -- ** Builders for 'LoggerConfig'
       , consoleOutB
       , mapperB
       , maybePrefixB
       , prefixB
       , productionB
       , showTidB
       , showTimeB
       ) where

import           Universum

import           Control.Lens           (at, makeLenses, zoom, _Just)
import           Control.Monad.State    (put)
import           Data.Aeson             (withObject)
import qualified Data.HashMap.Strict    as HM hiding (HashMap)
import           Data.Monoid            (Any (..))
import           Data.Text              (Text)
import qualified Data.Text.Buildable    as Buildable
import           Data.Traversable       (for)
import           Data.Word              (Word64)
import           Data.Yaml              (FromJSON (..), ToJSON (..), Value (Object),
                                         object, (.!=), (.:), (.:?), (.=))
import           Formatting             (bprint, shown)
import           GHC.Generics           (Generic)
import           System.FilePath        (normalise)

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

-- | Wrapper over file handler with additional rounding option.
data HandlerWrap = HandlerWrap
    { _hwFilePath :: !FilePath
      -- ^ Path to the file to be handled.
    , _hwRounding :: !(Maybe Int)
      -- ^ Amount of seconds to round time on (if set).
    } deriving (Generic,Show)

makeLenses ''HandlerWrap

type LoggerMap = HashMap Text LoggerTree

-- | Stores configuration for hierarchical loggers.
data LoggerTree = LoggerTree
    { _ltSubloggers :: !LoggerMap
    , _ltFiles      :: ![HandlerWrap]
    , _ltSeverity   :: !(Maybe Severity)
    } deriving (Generic, Show)

makeLenses ''LoggerTree

-- TODO: QuickCheck tests on monoid laws
instance Monoid LoggerTree where
    mempty = LoggerTree
        { _ltFiles      = []
        , _ltSeverity   = Nothing
        , _ltSubloggers = mempty
        }

    lt1 `mappend` lt2 = LoggerTree
        { _ltFiles      = _ltFiles      lt1 <>  _ltFiles      lt2
        , _ltSeverity   = _ltSeverity   lt1 <|> _ltSeverity   lt2
        , _ltSubloggers = _ltSubloggers lt1 <>  _ltSubloggers lt2
        }

instance ToJSON HandlerWrap
instance FromJSON HandlerWrap where
    -- we use 'normalise' so that Unix paths would work on Windows
    parseJSON = withObject "handler wrap" $ \o -> do
        (_hwFilePath :: FilePath) <- normalise <$> o .: "file"
        (_hwRounding :: Maybe Int) <- o .:? "round"
        pure HandlerWrap{..}

nonLoggers :: [Text]
nonLoggers = ["file", "files", "severity", "rounding", "handlers"]

instance ToJSON LoggerTree
instance FromJSON LoggerTree where
    parseJSON = withObject "loggers tree" $ \o -> do
        (singleFile :: Maybe FilePath) <- fmap normalise <$> o .:? "file"
        (manyFiles :: [FilePath]) <- map normalise <$> (o .:? "files" .!= [])
        handlers <- o .:? "handlers" .!= []
        let fileHandlers =
                map (\fp -> HandlerWrap fp Nothing) $
                maybe [] (:[]) singleFile ++ manyFiles
        let _ltFiles = fileHandlers <> handlers
        _ltSeverity   <- o .:? "severity"
        _ltSubloggers <- for (filterObject nonLoggers o) parseJSON
        return LoggerTree{..}

-- | Useful lens combinator to be used for logging initialization.
-- Usually should be used with 'zoomLogger'.
fromScratch :: Monoid m => State m a -> m
fromScratch = executingState mempty

-- | Zooming into logger name with putting specific key.
zoomLogger :: Text -> State LoggerTree () -> State LoggerTree ()
zoomLogger loggerName initializer = zoom (ltSubloggers.at loggerName) $ do
    put $ Just mempty
    zoom _Just initializer

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
      _lcRotation      :: Maybe RotationParameters

      -- | Severity for terminal output. If @Nothing@ then 'Warning' is used.
    , _lcTermSeverity  :: Maybe Severity

      -- | Show time for non-error messages.
      -- Note that error messages always have timestamp.
    , _lcShowTime      :: Any

      -- | Show 'ThreadId' for current logging thread.
    , _lcShowTid       :: Any

      -- | @True@ if we should also print output into console.
    , _lcConsoleOutput :: Any

      -- | Defines how to transform logger names in config.
    , _lcMapper        :: Endo LoggerName

      -- | Path prefix to add for each logger file
    , _lcFilePrefix    :: Maybe FilePath

      -- | Hierarchical tree of loggers.
    , _lcTree          :: LoggerTree
    }

makeLenses ''LoggerConfig

-- TODO: QuickCheck tests on monoid laws
instance Monoid LoggerConfig where
    mempty = LoggerConfig
        { _lcRotation      = Nothing
        , _lcTermSeverity  = Nothing
        , _lcShowTime      = mempty
        , _lcShowTid       = mempty
        , _lcConsoleOutput = mempty
        , _lcMapper        = mempty
        , _lcFilePrefix    = Nothing
        , _lcTree          = mempty
        }

    lc1 `mappend` lc2 = LoggerConfig
        { _lcRotation      = orCombiner  _lcRotation
        , _lcTermSeverity  = orCombiner  _lcTermSeverity
        , _lcShowTime      = andCombiner _lcShowTime
        , _lcShowTid       = andCombiner _lcShowTid
        , _lcConsoleOutput = andCombiner _lcConsoleOutput
        , _lcMapper        = andCombiner _lcMapper
        , _lcFilePrefix    = orCombiner  _lcFilePrefix
        , _lcTree          = andCombiner _lcTree
        }
      where
        orCombiner  field = field lc1 <|> field lc2
        andCombiner field = field lc1  <> field lc2


topLevelParams :: [Text]
topLevelParams =
    ["rotation", "showTime", "printOutput", "filePrefix" ]

instance FromJSON LoggerConfig where
    parseJSON = withObject "rotation params" $ \o -> do
        _lcRotation      <-         o .:? "rotation"
        _lcTermSeverity  <-         o .:? "termSeverity"
        _lcShowTime      <- Any <$> o .:? "showTime"    .!= False
        _lcShowTid       <- Any <$> o .:? "showTid"     .!= False
        _lcConsoleOutput <- Any <$> o .:? "printOutput" .!= False
        _lcFilePrefix    <-         o .:? "filePrefix"
        _lcTree          <- parseJSON $ Object $ filterObject topLevelParams o
        let _lcMapper     = mempty
        return LoggerConfig{..}

-- | This instances violates @fromJSON . toJSON = identity@ rule but doesn't matter
-- because it is used only for debugging.
instance ToJSON LoggerConfig where
    toJSON LoggerConfig{..} = object
            [ "rotation"     .= _lcRotation
            , "termSeverity" .= _lcTermSeverity
            , "showTime"     .= getAny _lcShowTime
            , "showTid"      .= getAny _lcShowTid
            , "printOutput"  .= getAny _lcConsoleOutput
            , "filePrefix"   .= _lcFilePrefix
            , ("logTree", toJSON _lcTree)
            ]

-- Builders for 'LoggerConfig'.

-- | Setup 'lcShowTime' to 'True' inside 'LoggerConfig'.
showTimeB :: LoggerConfig
showTimeB = mempty { _lcShowTime = Any True }

-- | Setup 'lcShowTid' to 'True' inside 'LoggerConfig'.
showTidB :: LoggerConfig
showTidB = mempty { _lcShowTid = Any True }

-- | Setup 'lcConsoleOutput' inside 'LoggerConfig'.
consoleOutB :: LoggerConfig
consoleOutB = mempty { _lcConsoleOutput = Any True }

-- | Adds sensible predefined set of parameters to logger.
productionB :: LoggerConfig
productionB = showTimeB <> consoleOutB

-- | Setup 'lcMapper' inside 'LoggerConfig'.
mapperB :: (LoggerName -> LoggerName) -> LoggerConfig
mapperB loggerNameMapper = mempty { _lcMapper = Endo loggerNameMapper }

-- | Setup 'lcFilePrefix' inside 'LoggerConfig' to optional prefix.
maybePrefixB :: Maybe FilePath -> LoggerConfig
maybePrefixB prefix = mempty { _lcFilePrefix = prefix }

-- | Setup 'lcFilePrefix' inside 'LoggerConfig' to specific prefix.
prefixB :: FilePath -> LoggerConfig
prefixB = maybePrefixB . Just
