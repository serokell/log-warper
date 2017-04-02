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
--       , lcMemModeLimit
       , lcRotation
       , lcShowTime
       , lcTermSeverity
       , lcTree
       , zoomLogger

         -- ** Builders for 'LoggerConfig'
       , consoleOutB
       , mapperB
--       , memoryB
       , prefixB
       , productionB
       , showTimeB
       ) where

import           Universum

import           Control.Lens           (at, makeLenses, zoom, _Just)
import           Control.Monad.State    (put)
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

-- | Useful lens combinator to be used for logging initialization.
fromScratch :: Monoid m => State m a -> m
fromScratch = executingState mempty

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
    parseJSON = withObject "handler wrap" $ \o -> do
        (_hwFilePath :: FilePath) <- o .: "file"
        (_hwRounding :: Maybe Int) <- o .:? "round"
        pure HandlerWrap{..}

nonLoggers :: [Text]
nonLoggers = ["file", "files", "severity", "rounding", "handlers"]

instance ToJSON LoggerTree
instance FromJSON LoggerTree where
    parseJSON = withObject "loggers tree" $ \o -> do
        (singleFile :: Maybe FilePath) <- o .:? "file"
        (manyFiles :: [FilePath]) <- o .:? "files" .!= []
        handlers <- o .:? "handlers" .!= []
        let fileHandlers =
                map (\fp -> HandlerWrap fp Nothing) $
                maybe [] (:[]) singleFile ++ manyFiles
        let _ltFiles = fileHandlers <> handlers
        _ltSeverity   <- o .:? "severity"
        _ltSubloggers <- for (filterObject nonLoggers o) parseJSON
        return LoggerTree{..}

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

      -- | @True@ if we should also print output into console.
    , _lcConsoleOutput :: Any

      -- | Defines how to transform logger names in config.
    , _lcMapper        :: Endo LoggerName

      -- | Path prefix to add for each logger file
    , _lcFilePrefix    :: Maybe FilePath

--      -- | Limit for queue in memory mode.
--    , _lcMemModeLimit  :: Maybe Word64

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
        , _lcConsoleOutput = mempty
        , _lcMapper        = mempty
        , _lcFilePrefix    = mempty
--        , _lcMemModeLimit  = Nothing
        , _lcTree          = mempty
        }

    lc1 `mappend` lc2 = LoggerConfig
        { _lcRotation      = _lcRotation      lc1 <|> _lcRotation      lc2
        , _lcTermSeverity  = _lcTermSeverity  lc1 <|> _lcTermSeverity  lc2
        , _lcShowTime      = _lcShowTime      lc1  <> _lcShowTime      lc2
        , _lcConsoleOutput = _lcConsoleOutput lc1  <> _lcConsoleOutput lc2
        , _lcMapper        = _lcMapper        lc1  <> _lcMapper        lc2
        , _lcFilePrefix    = _lcFilePrefix    lc1 <|> _lcFilePrefix    lc2
--        , _lcMemModeLimit  = _lcMemModeLimit  lc1 <|> _lcMemModeLimit  lc2
        , _lcTree          = _lcTree          lc1  <> _lcTree          lc2
        }

topLevelParams :: [Text]
topLevelParams =
    ["rotation", "showTime", "printOutput", "filePrefix", "roundTime"]

instance FromJSON LoggerConfig where
    parseJSON = withObject "rotation params" $ \o -> do
        _lcRotation      <-         o .:? "rotation"
        _lcTermSeverity  <-         o .:? "termSeverity"
        _lcShowTime      <- Any <$> o .:? "showTime"    .!= False
        _lcConsoleOutput <- Any <$> o .:? "printOutput" .!= False
        _lcFilePrefix    <-         o .:? "filePrefix"
--        _lcMemModeLimit  <-         o .:? "memModeLimit"
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
            , "printOutput"  .= getAny _lcConsoleOutput
            , "filePrefix"   .= _lcFilePrefix
--            , "memModeLimit" .= _lcMemModeLimit
            , ("logTree", toJSON _lcTree)
            ]

-- Builders for 'LoggerConfig'.

-- | Setup 'lcShowTime' inside 'LoggerConfig'.
showTimeB :: Bool -> LoggerConfig
showTimeB isShowTime = mempty { _lcShowTime = Any isShowTime }

-- | Setup 'lcConsoleOutput' inside 'LoggerConfig'.
consoleOutB :: Bool -> LoggerConfig
consoleOutB printToConsole = mempty { _lcConsoleOutput = Any printToConsole }

-- | Adds sensible predefined set of parameters to logger.
productionB :: LoggerConfig
productionB = showTimeB True <> consoleOutB True

-- | Setup 'lcMapper' inside 'LoggerConfig'.
mapperB :: (LoggerName -> LoggerName) -> LoggerConfig
mapperB loggerNameMapper = mempty { _lcMapper = Endo loggerNameMapper }

-- | Setup 'lcFilePrefix' inside 'LoggerConfig'.
prefixB :: FilePath -> LoggerConfig
prefixB filePrefix = mempty { _lcFilePrefix = Just filePrefix }

---- | Setup memory logger with certain limit
--memoryB :: Word64 -> LoggerConfig
--memoryB limit = mempty { _lcMemModeLimit = Just limit }
