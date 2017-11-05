{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Type class that add ability to log messages.
-- Supports pure and IO logging.
module System.Wlog.CanLog
       ( CanLog (..)
       , WithLogger

         -- * Pure logging manipulation
       , PureLogger (..)
       , LogEvent   (..)
       , dispatchEvents
       , logEvents
       , runPureLog
       , launchPureLog

       , NamedPureLogger (..)
       , runNamedPureLog
       , launchNamedPureLog

         -- * Logging functions
       , logDebug
       , logError
       , logInfo
       , logNotice
       , logWarning
       , logMessage
       ) where

import           Universum

import           Control.Monad.Except       (ExceptT)
import           Control.Monad.Morph        (MFunctor (..))
import qualified Control.Monad.RWS          as RWSLazy
import qualified Control.Monad.RWS.Strict   as RWSStrict
import qualified Control.Monad.State.Lazy   as StateLazy
import           Control.Monad.State.Strict (modify')
import           Control.Monad.Trans        (MonadTrans (lift))
import qualified Data.DList                 as DL (DList, snoc)
import           Data.SafeCopy              (base, deriveSafeCopySimple)

import           System.Wlog.Logger         (logM)
import           System.Wlog.LoggerName     (LoggerName (..))
import           System.Wlog.LoggerNameBox  (HasLoggerName (..), LoggerNameBox (..),
                                             usingLoggerName)
import           System.Wlog.Severity       (Severity (..))


-- | Type alias for constraints 'CanLog' and 'HasLoggerName'.
-- We need two different type classes to support more flexible interface
-- but in practice we usually use them both.
type WithLogger m = (CanLog m, HasLoggerName m)

-- | Instances of this class should explain how they add messages to their log.
class Monad m => CanLog m where
    dispatchMessage :: LoggerName -> Severity -> Text -> m ()

    -- Redundant constraint here due to type checker regressing in ghc-8.0.2-rc1
    -- https://ghc.haskell.org/trac/ghc/ticket/12784
    default dispatchMessage :: (MonadTrans t, t n ~ m, CanLog n)
                            => LoggerName
                            -> Severity
                            -> Text
                            -> m ()
    dispatchMessage name sev t = lift $ dispatchMessage name sev t

instance CanLog IO where
    dispatchMessage (getLoggerName -> name) prior msg = logM name prior msg

instance CanLog m => CanLog (LoggerNameBox m)
instance CanLog m => CanLog (ReaderT r m)
instance CanLog m => CanLog (StateT s m)
instance CanLog m => CanLog (StateLazy.StateT s m)
instance CanLog m => CanLog (ExceptT s m)
instance (CanLog m, Monoid w) => CanLog (RWSLazy.RWST r w s m)
instance (CanLog m, Monoid w) => CanLog (RWSStrict.RWST r w s m)

-- | Holds all required information for 'dispatchLoggerName' function.
data LogEvent = LogEvent
    { leLoggerName :: !LoggerName
    , leSeverity   :: !Severity
    , leMessage    :: !Text
    } deriving (Show)

deriveSafeCopySimple 0 'base ''LogEvent

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

-- | Shortcut for 'logMessage' to use according severity.
logDebug, logInfo, logNotice, logWarning, logError
    :: WithLogger m
    => Text -> m ()
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
    name <- askLoggerName
    dispatchMessage name severity t
