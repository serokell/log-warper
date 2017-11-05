{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}

-- | This module supports pure logging.
module System.Wlog.PureLogging
       (
       -- * Pure logging manipulation
         PureLogger (..)
       , LogEvent   (..)
       , dispatchEvents
       , logEvents
       , runPureLog
       , launchPureLog

       , NamedPureLogger (..)
       , runNamedPureLog
       , launchNamedPureLog
       ) where

import           Universum

import           Control.Monad.Morph        (MFunctor (..))
import           Control.Monad.State.Strict (modify')
import           Control.Monad.Trans        (MonadTrans (lift))
import qualified Data.DList                 as DL (DList, snoc)

import           System.Wlog.CanLog         (CanLog (..), WithLogger)
import           System.Wlog.HasLoggerName  (HasLoggerName (..))
import           System.Wlog.LoggerName     (LoggerName (..))
import           System.Wlog.LoggerNameBox  (LoggerNameBox (..), usingLoggerName)
import           System.Wlog.Severity       (Severity (..))

-- | Holds all required information for 'dispatchLoggerName' function.
data LogEvent = LogEvent
    { leLoggerName :: !LoggerName
    , leSeverity   :: !Severity
    , leMessage    :: !Text
    } deriving (Show)

-- | Pure implementation of 'CanLog' type class. Instead of writing log messages
-- into console it appends log messages into 'StateT' log. It uses 'DList' for
-- better performance, because messages can be added only at the end of log.
-- But it uses 'unsafePerformIO' so use with caution within IO.
--
-- TODO: Should we use some @Data.Tree@-like structure to observe message only
-- by chosen logger names?
newtype PureLogger m a = PureLogger
    { runPureLogger :: StateT (DL.DList LogEvent) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadState (DL.DList LogEvent),
                MonadThrow, HasLoggerName)

instance Monad m => CanLog (PureLogger m) where
    dispatchMessage leLoggerName leSeverity leMessage = modify' (flip DL.snoc LogEvent{..})

instance MFunctor PureLogger where
    hoist f = PureLogger . hoist f . runPureLogger

-- | Return log of pure logging action as list of 'LogEvent'.
runPureLog :: Monad m => PureLogger m a -> m (a, [LogEvent])
runPureLog = fmap (second toList) . flip runStateT mempty . runPureLogger

-- | Logs all 'LogEvent'`s from given list. This function supposed to
-- be used after 'runPureLog'.
dispatchEvents :: CanLog m => [LogEvent] -> m ()
dispatchEvents = mapM_ dispatchLogEvent
  where
    dispatchLogEvent (LogEvent name sev t) = dispatchMessage name sev t

-- | Logs all 'LogEvent'`s from given list. Just like
-- 'dispatchEvents' but uses proper logger Name.
logEvents :: WithLogger m => [LogEvent] -> m ()
logEvents events = do
    logName <- askLoggerName
    mapM_ (dispatchLogEvent logName) events
  where
    dispatchLogEvent logName (LogEvent _ sev t) = dispatchMessage logName sev t

-- | Performs actual logging once given action completes.
launchPureLog
    :: (CanLog n, Monad m)
    => (forall f. Functor f => m (f a) -> n (f b))
    -> PureLogger m a
    -> n b
launchPureLog hoist' action = do
    (logs, res) <- hoist' $ swap <$> runPureLog action
    res <$ dispatchEvents logs

newtype NamedPureLogger m a = NamedPureLogger
    { runNamedPureLogger :: PureLogger (LoggerNameBox m) a
    } deriving (Functor, Applicative, Monad, MonadState (DL.DList LogEvent),
                MonadThrow, HasLoggerName)

instance MonadTrans NamedPureLogger where
    lift = NamedPureLogger . lift . lift

instance Monad m => CanLog (NamedPureLogger m) where
    dispatchMessage name sev msg =
        NamedPureLogger $ dispatchMessage name sev msg

instance MFunctor NamedPureLogger where
    hoist f = NamedPureLogger . hoist (hoist f) . runNamedPureLogger

-- | Return log of pure logging action as list of 'LogEvent',
-- using logger name provided by context.
runNamedPureLog
    :: (Monad m, HasLoggerName m)
    => NamedPureLogger m a -> m (a, [LogEvent])
runNamedPureLog (NamedPureLogger action) =
    askLoggerName >>= (`usingLoggerName` runPureLog action)

-- | Similar as `launchPureLog`, but provides logger name from current context.
launchNamedPureLog
    :: (WithLogger n, Monad m)
    => (forall f. Functor f => m (f a) -> n (f b))
    -> NamedPureLogger m a
    -> n b
launchNamedPureLog hoist' (NamedPureLogger action) = do
    name <- askLoggerName
    (logs, res) <- hoist' $ swap <$> usingLoggerName name (runPureLog action)
    res <$ dispatchEvents logs
