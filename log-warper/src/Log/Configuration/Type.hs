{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

{- | Logger configuration for application. This configuration can be read from
@.yaml@ file (and we hope to replace YAML with TOML in future). This whole
configuration is stored inside 'ReaderT' configuration.
-}

module Log.Configuration.Type
       ( -- * Configuration type
         Configuration (..)
       , cRotation
       , cGlobal
       , cTree

         -- * Global parameters
       , GlobalParameters (..)
       , defaultGlobalParameters
       , gpShowTime
       , gpShowTid
       , gpLogsDirectory

       , LoggerTree (..)
       , unLoggerTree

       , LoggerConfiguration (..)
       , lcSeverities
       , lcFile

--       , fromScratch
--       , zoomLogger
--       , atLogger
       ) where

import Universum

import Data.Traversable (for)
import Data.Yaml (FromJSON (..), ToJSON, Value (Object), withObject, (.!=), (.:?))
import Lens.Micro.Platform (makeLenses)
import System.FilePath (normalise)

import Log.Configuration.Extension (Extension (..))
import Log.Configuration.Rotation (RotationExtension)
import Log.Name (LoggerName (..))
import Log.Severity (Severities, severitiesJsonP)

import qualified Data.HashMap.Strict as HashMap hiding (HashMap)

----------------------------------------------------------------------------
-- Top-level logging configuration
----------------------------------------------------------------------------

{- | Configuration for flexible and hierarchical logging. Logging configuration contains multiple parts:

* 'GlobalParameters': description of logging parameters applied to every logger.
* 'LoggerTree': per-logger configration.
* 'Log.Configuration.Rotation.RotationParameters': parameters for logger rotation. Allowed to not be specified.

Behavior of YAML parsing for any extension point (for example, 'Rotation') is
the following: parsing succeeds iff extension and config fields are both present
or both absent.

-}
data Configuration (exts :: [Extension]) = Configuration
    { _cRotation :: !(RotationExtension exts)
    , _cGlobal   :: !GlobalParameters
    , _cTree     :: !LoggerTree
    } deriving (Generic)

deriving instance (Show (RotationExtension exts)) => Show (Configuration exts)
deriving instance (Eq   (RotationExtension exts)) => Eq   (Configuration exts)

instance (ToJSON (RotationExtension exts)) => ToJSON (Configuration exts)

{- |
In YAML file this should be specified like this:

@
rotation:  # required only if specified by action, otherwise mustn't be specified
    ... 'RotationParameters' ...
global:
    ... 'GlobalParameters' ...
tree:
    ... 'LoggerTree' ...
@
-}
instance (FromJSON (RotationExtension exts)) => FromJSON (Configuration exts) where
    parseJSON = withObject "Config" $ \o -> do
        _cRotation <- parseJSON (Object o)
        _cGlobal <- o .:? "global" .!= defaultGlobalParameters
        _cTree   <- o .:? "tree"   .!= mempty
        pure Configuration{..}

----------------------------------------------------------------------------
-- Global parameters for logging
----------------------------------------------------------------------------

-- | Global logger configuration for all loggers.
data GlobalParameters = GlobalParameters
    { -- | Set 'True' if you want to see UTC time for log messages.
      _gpShowTime      :: Bool

      -- | Set 'True' if you want to see 'ThreadId' for current logging thread.
    , _gpShowTid       :: Bool

      -- | Specifies directory for log files. This can be useful to avoid
      -- prefixes if you have a lot of loggers. Another use case: different logger
     -- directories on different platforms.
    , _gpLogsDirectory :: Maybe FilePath
    } deriving (Show, Eq, Generic)

instance ToJSON GlobalParameters

{- | In YAML file this should be specified like this:

@
showTime: false # 'True' by default
showTid: true   # 'False' by default
logsDirectory: logs
@

Every field is optional.
-}
instance FromJSON GlobalParameters where
    parseJSON = withObject "GlobalParameters" $ \o -> do
        _gpShowTime      <- o .:? "showTime" .!= True
        _gpShowTid       <- o .:? "showTid"  .!= False
        _gpLogsDirectory <- o .:? "logsDirectory"
        return GlobalParameters{..}

{- | Default 'GlobalParameters'.

1. No logging directory.
2. Logs messages with time.
3. Logs messages without 'ThreadId'.
-}
defaultGlobalParameters :: GlobalParameters
defaultGlobalParameters = GlobalParameters
    { _gpShowTime      = True
    , _gpShowTid       = False
    , _gpLogsDirectory = Nothing
    }

----------------------------------------------------------------------------
-- Logger tree
----------------------------------------------------------------------------

{- | Configuration for logger with some 'LoggerName'. Contains severity level
and possible file to log to.
-}
data LoggerConfiguration = LoggerConfiguration
    { _lcSeverities :: Severities
    , _lcFile       :: Maybe FilePath
    } deriving (Show, Eq, Generic)

instance ToJSON LoggerConfiguration

{- | Stores configuration for hierarchical loggers. Hierarchical logging means
that logger with name @foo@ is a parent of logger @foo.bar@. And if logger
@parent.child@ doesn't have associated with it configuration it takes this
configuration from logger with name @parent@. Which means that loggers actually
form tree. But in practice all loggers are stored in flat map for performance
optmization purposes. Logger name with name 'mempty' or just @""@ is called @rootLoggerName@.

__High restriction!__ You can't have logger components with name @file@ or @severity@.

-}
newtype LoggerTree = LoggerTree
    { _unLoggerTree :: HashMap LoggerName LoggerConfiguration
    } deriving (Generic, Show, Eq, Semigroup, Monoid, ToPairs)

-- TODO: make _LoggerTree lens manually instead of generated unLoggerTree lens

instance ToJSON LoggerTree

{- | In YAML this may looks like this:

@
severity: Warning+     # severity for "root" logger, no file given
app:                   # configuration for logger "app"
  severity: Info+      # severity for logger "app"
  file: app.log        # autput file for loggers "app" and "app.*"
  db.low:              # configuration for logger "app.db.low"; __note:__ there's no "app.db" logger
    severity: Debug+   # severity for logger "db.low"
  db.hi:               # configuration for logger "app.db.hi"
    severity: Notice+  # severity for "app.db.hi" logger
networking:            # configuration for logger "networking"
  severity: Debug+     # severity for logger "networking", no file given
@
-}
instance FromJSON LoggerTree where
    parseJSON = fmap flattenTree . parseJSON @LoggerTreeInternal

-- | Intermediate data structure to parse from JSON. It's easier to parse to
-- this structure but it's more convenient to work with 'LoggerTree' in future.
data LoggerTreeInternal = LoggerTreeInternal
    { ltiConfiguration :: LoggerConfiguration
    , ltiMap           :: HashMap Text LoggerTreeInternal
    }

nonLoggers :: [Text]
nonLoggers = ["file", "severity"]

filterObject :: [Text] -> HashMap Text a -> HashMap Text a
filterObject excluded = HashMap.filterWithKey (\k _ -> k `notElem` excluded)

instance FromJSON LoggerTreeInternal where
    parseJSON = withObject "LoggerTree" $ \o -> do
        _lcSeverities <- severitiesJsonP o "severity"
        _lcFile :: Maybe FilePath <- normalise <<$>> o .:? "file"
        ltiMap <- for (filterObject nonLoggers o) parseJSON
        pure LoggerTreeInternal{ ltiConfiguration = LoggerConfiguration{..}
                               , ..
                               }

flattenTree :: LoggerTreeInternal -> LoggerTree
flattenTree = LoggerTree . flatTraverse mempty
  where
    toLoggerName :: Text -> LoggerName
    toLoggerName = fromString . toString

    flatTraverse :: Text -> LoggerTreeInternal -> HashMap LoggerName LoggerConfiguration
    flatTraverse (toLoggerName -> parent) (LoggerTreeInternal conf tree) = do
      -- flatten every subtree and join into single HashMap
      let flattenedTree = foldMap (uncurry flatTraverse) $ toPairs tree

      -- add parent prefix to each key
      let parentedTrees = map (first (parent <>)) $ toPairs flattenedTree

      -- insert "parent" config in result
      HashMap.insert parent conf $ HashMap.fromList parentedTrees

----------------------------------------------------------------------------
-- Generating all lenses in one place
----------------------------------------------------------------------------

makeLenses ''Configuration
makeLenses ''GlobalParameters
makeLenses ''LoggerTree
makeLenses ''LoggerConfiguration

----------------------------------------------------------------------------
-- EDSL for logger configuration
----------------------------------------------------------------------------

-- -- | Useful lens combinator to be used for logging initialization.
-- -- Usually should be used with 'zoomLogger'.
-- fromScratch :: Monoid m => State m a -> m
-- fromScratch = executingState mempty
--
-- -- | Zooming into logger name with putting specific key.
-- zoomLogger :: LoggerName -> State LoggerTree () -> State LoggerTree ()
-- zoomLogger loggerName initializer = zoom (ltSubloggers.at loggerName) $ do
--     put $ Just mempty
--     zoom _Just initializer
--
-- -- | Lens to help to change some particular logger properties.
-- --
-- -- For example if you want to use default configurations,
-- -- but need to change logger's severity to 'warningPlus'
-- -- you can do it this way:
-- --
-- -- @
-- -- 'System.Wlog.Launcher.launchWithConfig'
-- --     ( defaultConfig "myLogger"
-- --     & 'atLogger' "myLogger"
-- --     . 'ltSeverity' ?~ 'warningPlus' )
-- --     "myLogger"
-- --     action
-- -- @
-- --
-- atLogger :: LoggerName -> Traversal' LoggerConfig LoggerTree
-- atLogger logName = lcTree . leveldown (LoggerName <$> Text.splitOn "." (getLoggerName logName))
--   where
--     leveldown :: [LoggerName] -> Traversal' LoggerTree LoggerTree
--     leveldown []     = bug EmptyLoggerName
--     leveldown [x]    = getSublogger x
--     leveldown (x:xs) = getSublogger x . leveldown xs
--
--     getSublogger :: LoggerName -> Traversal' LoggerTree LoggerTree
--     getSublogger x = ltSubloggers . at x . _Just
--
-- -- | Exceptions for handling logger exceptions with lens.
-- data LoggerLensException = EmptyLoggerName
--
-- instance Exception LoggerLensException
--
-- instance Show LoggerLensException where
--     show EmptyLoggerName = "Logger name should be provided"
