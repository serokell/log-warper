{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
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
       , lcConsoleAction
       , lcLogsDirectory
       , lcMapper
       , lcRotation
       , lcShowTime
       , lcShowTid
       , lcTermSeverityOut
       , lcTermSeverityErr
       , lcTree
       , zoomLogger
       , atLogger

         -- ** Builders for 'LoggerConfig'
       , consoleActionB
       , customConsoleActionB
       , mapperB
       , maybeLogsDirB
       , logsDirB
       , productionB
       , showTidB
       , showTimeB
       , termSeveritiesOutB
       , termSeveritiesErrB
       ) where

import Universum

import Data.Aeson (withObject)
import Data.Traversable (for)
import Data.Yaml (FromJSON (..), Object, Parser, ToJSON (..), Value (..), object, (.!=), (.:),
                  (.:?), (.=))
import Fmt (Buildable, build, (||+))
import Lens.Micro.Platform (at, makeLenses, zoom, _Just)
import System.FilePath (normalise)

import System.Wlog.LoggerName (LoggerName (..))
import System.Wlog.LogHandler.Simple (defaultHandleAction)
import System.Wlog.Severity (Severities, allSeverities, debugPlus, errorPlus, infoPlus, noticePlus,
                             warningPlus)

import qualified Data.HashMap.Strict as HM hiding (HashMap)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified GHC.Show as Show

----------------------------------------------------------------------------
-- Utilites & helpers
----------------------------------------------------------------------------

filterObject :: [Text] -> HashMap Text a -> HashMap LoggerName a
filterObject excluded = HM.fromList . map (first LoggerName) . HM.toList . HM.filterWithKey (\k _ -> k `notElem` excluded)

parseSeverities :: Object -> Text -> Parser (Maybe Severities)
parseSeverities o term =
    case HM.lookup term o of
        Just value -> case value of
            String word -> case word of
                "All"      -> pure $ Just allSeverities
                "Debug+"   -> pure $ Just debugPlus
                "Info+"    -> pure $ Just infoPlus
                "Notice+"  -> pure $ Just noticePlus
                "Warning+" -> pure $ Just warningPlus
                "Error+"   -> pure $ Just errorPlus
                _          -> fail $ toString $ "Unknown severity: " <> word
            Array sevs  -> Just . Set.fromList . Vector.toList <$> Vector.mapM parseJSON sevs
            _           -> fail "Incorrect severities format"
        Nothing    -> pure Nothing

----------------------------------------------------------------------------
-- LoggerTree
----------------------------------------------------------------------------

-- | Wrapper over file handler with additional rounding option.
data HandlerWrap = HandlerWrap
    { _hwFilePath :: !FilePath
      -- ^ Path to the file to be handled.
    , _hwRounding :: !(Maybe Int)
      -- ^ Round timestamps to this power of 10 picoseconds.
      -- Just 3 would round to nanoseconds.
      -- Just 12 would round to seconds.
    } deriving (Generic,Show)

makeLenses ''HandlerWrap

type LoggerMap = HashMap LoggerName LoggerTree

-- | Stores configuration for hierarchical loggers.
data LoggerTree = LoggerTree
    { _ltSubloggers :: !LoggerMap
    , _ltFiles      :: ![HandlerWrap]
    , _ltSeverity   :: !(Maybe Severities)
    } deriving (Generic, Show)

makeLenses ''LoggerTree

instance Semigroup LoggerTree where
    lt1 <> lt2 = LoggerTree
        { _ltFiles      = andCombiner _ltFiles
        , _ltSeverity   = orCombiner  _ltSeverity
        , _ltSubloggers = andCombiner _ltSubloggers
        }
      where
        orCombiner  field = field lt1 <|> field lt2
        andCombiner field = field lt1  <> field lt2

-- TODO: QuickCheck tests on monoid laws
instance Monoid LoggerTree where
    mempty = LoggerTree
        { _ltFiles      = []
        , _ltSeverity   = Nothing
        , _ltSubloggers = mempty
        }

    mappend = (<>)

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
                map (`HandlerWrap` Nothing) $
                maybeToList singleFile ++ manyFiles
        let _ltFiles = fileHandlers <> handlers
        _ltSeverity   <- parseSeverities o "severity"
        _ltSubloggers <- for (filterObject nonLoggers o) parseJSON
        return LoggerTree{..}

-- | Useful lens combinator to be used for logging initialization.
-- Usually should be used with 'zoomLogger'.
fromScratch :: Monoid m => State m a -> m
fromScratch = executingState mempty

-- | Zooming into logger name with putting specific key.
zoomLogger :: LoggerName -> State LoggerTree () -> State LoggerTree ()
zoomLogger loggerName initializer = zoom (ltSubloggers.at loggerName) $ do
    put $ Just mempty
    zoom _Just initializer

----------------------------------------------------------------------------
-- Logger rotation
----------------------------------------------------------------------------

-- | Parameters for logging rotation.
data RotationParameters = RotationParameters
    { rpLogLimit  :: !Word64  -- ^ max size of file in bytes
    , rpKeepFiles :: !Word    -- ^ number of files to keep
    } deriving (Generic, Show)

instance Buildable RotationParameters where
    build x = x||+""

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
      _lcRotation        :: Maybe RotationParameters

      -- | Severity for terminal `stdout` output. If @Nothing@ along
      --   with '_lcTermSeverityErr' then 'Warning' and greater
      --   excluding 'Error' are used.
    , _lcTermSeverityOut :: Maybe Severities

      -- | Severity for terminal `stderr` output. If @Nothing@ along
      --   with '_lcTermSeverityOut' then 'Error' is used.
    , _lcTermSeverityErr :: Maybe Severities

      -- | Show time for non-error messages.
      -- Note that error messages always have timestamp.
    , _lcShowTime        :: Any

      -- | Show 'ThreadId' for current logging thread.
    , _lcShowTid         :: Any

      -- | Specifies action for printing to console.
    , _lcConsoleAction   :: Last (Handle -> Text -> IO ())

      -- | Defines how to transform logger names in config.
    , _lcMapper          :: Endo LoggerName

      -- | Specifies directory for log files. This can be useful to avoid
      -- prefixes if you have a lot of loggers. Another use case: different logger
     -- directories on different platforms.
    , _lcLogsDirectory   :: Maybe FilePath

      -- | Hierarchical tree of loggers.
    , _lcTree            :: LoggerTree
    }

makeLenses ''LoggerConfig

instance Semigroup LoggerConfig where
    lc1 <> lc2 = LoggerConfig
        { _lcRotation        = orCombiner  _lcRotation
        , _lcTermSeverityOut = orCombiner  _lcTermSeverityOut
        , _lcTermSeverityErr = orCombiner  _lcTermSeverityErr
        , _lcShowTime        = andCombiner _lcShowTime
        , _lcShowTid         = andCombiner _lcShowTid
        , _lcConsoleAction   = andCombiner _lcConsoleAction
        , _lcMapper          = andCombiner _lcMapper
        , _lcLogsDirectory   = orCombiner  _lcLogsDirectory
        , _lcTree            = andCombiner _lcTree
        }
      where
        orCombiner  field = field lc1 <|> field lc2
        andCombiner field = field lc1  <> field lc2


-- TODO: QuickCheck tests on monoid laws
instance Monoid LoggerConfig where
    mempty = LoggerConfig
        { _lcRotation        = Nothing
        , _lcTermSeverityOut = Nothing
        , _lcTermSeverityErr = Nothing
        , _lcShowTime        = mempty
        , _lcShowTid         = mempty
        , _lcConsoleAction   = mempty
        , _lcMapper          = mempty
        , _lcLogsDirectory   = Nothing
        , _lcTree            = mempty
        }

    mappend = (<>)

instance FromJSON LoggerConfig where
    parseJSON = withObject "rotation params" $ \o -> do
        _lcRotation        <-         o .:? "rotation"
        _lcTermSeverityOut <- parseSeverities o "termSeveritiesOut"
        _lcTermSeverityErr <- parseSeverities o "termSeveritiesErr"
        _lcShowTime        <- Any <$> o .:? "showTime"    .!= False
        _lcShowTid         <- Any <$> o .:? "showTid"     .!= False
        _lcLogsDirectory   <-         o .:? "filePrefix"  -- TODO: this field should be named "logsDirectory" but we keep previous name for backwards compatibility
        _lcTree            <-         o .:? "loggerTree"  .!= mempty

        printConsoleFlag    <- o .:? "printOutput" .!= False
        let _lcConsoleAction = Last $ bool Nothing (Just defaultHandleAction) printConsoleFlag
        let _lcMapper        = mempty
        return LoggerConfig{..}

-- | This instances violates @fromJSON . toJSON = identity@ rule but doesn't matter
-- because it is used only for debugging.
instance ToJSON LoggerConfig where
    toJSON LoggerConfig{..} = object
            [ "rotation"          .= _lcRotation
            , "termSeveritiesOut" .= _lcTermSeverityOut
            , "termSeveritiesErr" .= _lcTermSeverityErr
            , "showTime"          .= getAny _lcShowTime
            , "showTid"           .= getAny _lcShowTid
            , "printOutput"       .= isJust (getLast _lcConsoleAction)
            , "filePrefix"        .= _lcLogsDirectory
            , ("logTree", toJSON _lcTree)
            ]
----------------------------------------------------------------------------
-- Builders for 'LoggerConfig'.
----------------------------------------------------------------------------

-- | Setup 'lcTermSeverityOut' to specified severity inside 'LoggerConfig'.
termSeveritiesOutB :: Severities -> LoggerConfig
termSeveritiesOutB severities = mempty { _lcTermSeverityOut = Just severities }

-- | Setup 'lcTermSeverityErr' to specified severity inside 'LoggerConfig'.
termSeveritiesErrB :: Severities -> LoggerConfig
termSeveritiesErrB severities = mempty { _lcTermSeverityErr = Just severities }

-- | Setup 'lcShowTime' to 'True' inside 'LoggerConfig'.
showTimeB :: LoggerConfig
showTimeB = mempty { _lcShowTime = Any True }

-- | Setup 'lcShowTid' to 'True' inside 'LoggerConfig'.
showTidB :: LoggerConfig
showTidB = mempty { _lcShowTid = Any True }

consoleActionB :: (Handle -> Text -> IO ()) -> LoggerConfig
consoleActionB action = mempty { _lcConsoleAction = Last $ Just action }

-- | Setup 'lcConsoleOutput' inside 'LoggerConfig'.
customConsoleActionB :: Maybe (Handle -> Text -> IO ()) -> LoggerConfig
customConsoleActionB action = mempty { _lcConsoleAction = Last action }

-- | Adds sensible predefined set of parameters to logger.
productionB :: LoggerConfig
productionB = showTimeB <> customConsoleActionB (Just defaultHandleAction)

-- | Setup 'lcMapper' inside 'LoggerConfig'.
mapperB :: (LoggerName -> LoggerName) -> LoggerConfig
mapperB loggerNameMapper = mempty { _lcMapper = Endo loggerNameMapper }

-- | Setup 'lcLogsDirectory' inside 'LoggerConfig' to optional prefix.
maybeLogsDirB :: Maybe FilePath -> LoggerConfig
maybeLogsDirB prefix = mempty { _lcLogsDirectory = prefix }

-- | Setup 'lcLogsDirectory' inside 'LoggerConfig' to specific prefix.
logsDirB :: FilePath -> LoggerConfig
logsDirB = maybeLogsDirB . Just

-- | Lens to help to change some particular logger properties.
--
-- For example if you want to use default configurations,
-- but need to change logger's severity to 'warningPlus'
-- you can do it this way:
--
-- @
-- 'System.Wlog.Launcher.launchWithConfig'
--     ( defaultConfig "myLogger"
--     & 'atLogger' "myLogger"
--     . 'ltSeverity' ?~ 'warningPlus' )
--     "myLogger"
--     action
-- @
--
atLogger :: LoggerName -> Traversal' LoggerConfig LoggerTree
atLogger logName = lcTree . leveldown (LoggerName <$> Text.splitOn "." (getLoggerName logName))
  where
    leveldown :: [LoggerName] -> Traversal' LoggerTree LoggerTree
    leveldown []     = bug EmptyLoggerName
    leveldown [x]    = getSublogger x
    leveldown (x:xs) = getSublogger x . leveldown xs

    getSublogger :: LoggerName -> Traversal' LoggerTree LoggerTree
    getSublogger x = ltSubloggers . at x . _Just

-- | Exceptions for handling logger exceptions with lens.
data LoggerLensException = EmptyLoggerName

instance Exception LoggerLensException

instance Show LoggerLensException where
    show EmptyLoggerName = "Logger name should be provided"
