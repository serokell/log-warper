{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Type class that add ability to log messages.
-- Supports pure and IO logging.
module System.Wlog.CanLog
       ( CanLog (..)
       , WithLogger
       , memoryLogs
       , readMemoryLogs

         -- * Pure logging manipulation
       , PureLogger (..)
       , LogEvent   (..)
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

import           Control.Monad.Base        (MonadBase)
import           Control.Monad.Except      (ExceptT, MonadError)
import qualified Control.Monad.RWS         as RWSLazy
import qualified Control.Monad.RWS.Strict  as RWSStrict
import           Control.Monad.Trans       (MonadTrans (lift))
import           Control.Monad.Writer      (MonadWriter (tell), WriterT (runWriterT))
import qualified Data.DList                as DL (DList)
import           Data.SafeCopy             (base, deriveSafeCopySimple)
import           Data.Time                 (getCurrentTime)
import           System.IO.Unsafe          (unsafePerformIO)
import           Universum

import           System.Wlog.Formatter     (formatLogMessageColors, getRoundedTime)
import           System.Wlog.Logger        (logM)
import           System.Wlog.LoggerName    (LoggerName (..))
import           System.Wlog.LoggerNameBox (HasLoggerName (..), LoggerNameBox (..))
import           System.Wlog.MemoryQueue   (MemoryQueue)
import qualified System.Wlog.MemoryQueue   as MQ
import           System.Wlog.Severity      (Severity (..))


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
                            -> t n ()
    dispatchMessage name sev t = lift $ dispatchMessage name sev t

type LogMemoryQueue = MemoryQueue Text

-- TODO: dirty hack to have in-memory logs. Maybe will be refactored
-- later.  Maybe not.
memoryLogs :: MVar (Maybe (LogMemoryQueue,Maybe Int))
memoryLogs = unsafePerformIO $ newMVar Nothing
{-# NOINLINE memoryLogs #-}

-- | Retrieves memory logs in reversed order (newest are head).
readMemoryLogs :: (MonadIO m) => m [Text]
readMemoryLogs = do
    liftIO (readMVar memoryLogs) <&> maybe (pure []) (MQ.toList . fst)

instance CanLog IO where
    dispatchMessage (loggerName -> name) prior msg = logM name prior msg

instance CanLog m => CanLog (LoggerNameBox m)
instance CanLog m => CanLog (ReaderT r m)
instance CanLog m => CanLog (StateT s m)
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
-- into console it appends log messages into 'WriterT' log. It uses 'DList' for
-- better performance, because messages can be added only at the end of log.
-- But it uses 'unsafePerformIO' so use with caution within IO.
--
-- TODO: Should we use some @Data.Tree@-like structure to observe message only
-- by chosen logger names?
newtype PureLogger m a = PureLogger
    { runPureLogger :: WriterT (DL.DList LogEvent) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadWriter (DL.DList LogEvent),
                MonadBase b, MonadState s, MonadReader r, MonadError e, HasLoggerName)

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
    name <- getLoggerName
    dispatchMessage name severity t
    !() <- pure $ unsafePerformIO $ do
        let formatted r = do
                curTime <- maybe getCurrentTime getRoundedTime r
                pure $ formatLogMessageColors name severity curTime t
        let modif _ Nothing  = pure Nothing
            modif x (Just s) = Just <$> x s
        modifyMVar_ memoryLogs $ modif $ \(q,rv) -> do
            f <- formatted rv
            pure $ (MQ.pushFront f q, rv)
    pure ()
