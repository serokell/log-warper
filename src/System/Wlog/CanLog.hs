{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Type class that add ability to log messages.
-- Supports pure and IO logging.

module System.Wlog.CanLog
       ( CanLog (..)
       , WithLogger

         -- * Pure logging manipulation
       , PureLogger (..)
       , dispatchEvents
       , runPureLog

         -- * Logging functions
       , logDebug
       , logError
       , logInfo
       , logNotice
       , logWarning
       , logMessage
       ) where

import           Control.Monad.Reader      (MonadReader, ReaderT)
import           Control.Monad.State       (MonadState, StateT)
import           Control.Monad.Trans       (MonadTrans (lift))
import           Control.Monad.Writer      (MonadWriter (tell), WriterT (runWriterT))

import           Data.Bifunctor            (second)
import           Data.DList                (DList, toList)
import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           System.Log.Logger         (logM)

import           System.Wlog.LoggerName    (LoggerName (..))
import           System.Wlog.LoggerNameBox (HasLoggerName (..), LoggerNameBox (..))
import           System.Wlog.Severity      (Severity (..), convertSeverity)

-- | Type alias for constraints 'CanLog' and 'HasLoggerName'.
-- We need two different type classes to support more flexible interface
-- but in practice we usually use them both.
type WithLogger m = (CanLog m, HasLoggerName m)

-- | Instances of this class should explain how they add messages to their log.
class Monad m => CanLog m where
    dispatchMessage :: LoggerName -> Severity -> Text -> m ()

    default dispatchMessage :: MonadTrans t => LoggerName -> Severity -> Text -> t m ()
    dispatchMessage name sev t = lift $ dispatchMessage name sev t

instance CanLog IO where
    dispatchMessage
        (loggerName      -> name)
        (convertSeverity -> prior)
        (T.unpack        -> t)
      = logM name prior t

instance CanLog m => CanLog (LoggerNameBox m)
instance CanLog m => CanLog (ReaderT r m)
instance CanLog m => CanLog (StateT s m)

-- | Holds all required information for 'dispatchLoggerName' function.
data LogEvent = LogEvent
    { leLoggerName :: !LoggerName
    , leSeverity   :: !Severity
    , leMessage    :: !Text
    } deriving (Show)

-- | Pure implementation of 'CanLog' type class. Instead of writing log messages
-- into console it appends log messages into 'WriterT' log. It uses 'DList' for
-- better performance, because messages can be added only at the end of log.
-- But it uses 'unsafePerformIO' so use with caution within IO.
--
-- TODO: Should we use some @Data.Tree@-like structure to observe message only
-- by chosen loger names?
newtype PureLogger m a = PureLogger
    { runPureLogger :: WriterT (DList LogEvent) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadWriter (DList LogEvent),
                MonadState s, MonadReader r, HasLoggerName)

instance Monad m => CanLog (PureLogger m) where
    dispatchMessage leLoggerName leSeverity leMessage = tell [LogEvent{..}]

-- | Return log of pure logging action as list of 'LogEvent'.
runPureLog :: Monad m => PureLogger m a -> m (a, [LogEvent])
runPureLog = fmap (second toList) . runWriterT . runPureLogger

-- | Logs all 'LogEvent'`s from given list. This function supposed to
-- be used after 'runPureLog'.
dispatchEvents :: WithLogger m => [LogEvent] -> m ()
dispatchEvents = mapM_ dispatchLogEvent
  where
    dispatchLogEvent (LogEvent name sev t) = dispatchMessage name sev t

-- | Shortcut for 'logMessage' to use according severity.
logDebug, logInfo, logNotice, logWarning, logError
    :: WithLogger m
    => Text
    -> m ()
logDebug   = logMessage Debug
logInfo    = logMessage Info
logNotice  = logMessage Notice
logWarning = logMessage Warning
logError   = logMessage Error

-- | Logs message with specified severity using logger name in context.
logMessage
    :: WithLogger m
    => Severity
    -> Text
    -> m ()
logMessage severity t = do
    name <- getLoggerName
    dispatchMessage name severity t
