{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Type class that add ability to log messages.
-- Supports pure and IO logging.
module System.Wlog.CanLog
       ( CanLog (..)
       , WithLogger
       , WithLoggerIO

       -- * Logging functions
       , logDebug
       , logError
       , logInfo
       , logNotice
       , logWarning
       , logMessage

       , liftLogIO
       ) where

import Universum

import System.Wlog.HasLoggerName (HasLoggerName (..))
import System.Wlog.IOLogger (logM)
import System.Wlog.LoggerName (LoggerName (..))
import System.Wlog.LoggerNameBox (LoggerNameBox (..), usingLoggerName)
import System.Wlog.Severity (Severity (..))

import qualified Control.Monad.RWS as RWSLazy
import qualified Control.Monad.RWS.Strict as RWSStrict
import qualified Control.Monad.State.Lazy as StateLazy


-- | Type alias for constraints 'CanLog' and 'HasLoggerName'.
-- We need two different type classes to support more flexible interface
-- but in practice we usually use them both.
type WithLogger m = (CanLog m, HasLoggerName m)

-- | Type alias for constraints 'WithLogger' and 'MonadIO'.
-- It is a very common situation to use both of them together.
type WithLoggerIO m = (MonadIO m, WithLogger m)

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
    dispatchMessage = logM

instance CanLog m => CanLog (LoggerNameBox m)
instance CanLog m => CanLog (ReaderT r m)
instance CanLog m => CanLog (StateT s m)
instance CanLog m => CanLog (StateLazy.StateT s m)
instance CanLog m => CanLog (ExceptT s m)
instance (CanLog m, Monoid w) => CanLog (RWSLazy.RWST r w s m)
instance (CanLog m, Monoid w) => CanLog (RWSStrict.RWST r w s m)

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

{- | It's very common situation when we need to write log messages
inside functions that work in 'IO'.

To do so we need to configure logger name each time inside work of this function.

@liftLogIO@ is the easiest way to deal with such kind of situations.

==== __Usage Example__

We have some function

@
clientStart :: HostName -> .. -> IO ClientComponentMap
clientStart hostName .. = do
    ...
    forkIO $ routeIncoming endPoint msgs
    ...
@

We need to log 'Error' messages in @routeIncoming@ function.
To do that we firstly need @clientStart@ to work in 'MonadIO'

@
clientStart :: (MonadIO m) => HostName -> .. -> m ClientComponentMap
clientStart hostName .. = do
    ...
    liftIO $ forkIO $ routeIncoming endPoint msgs
    ...
@

After we added some 'logError' into @routeIncoming@ @clientStart@ should
now work in 'WithLogger'. Thus we can use 'WithLoggerIO' which is combination of 'MonadIO' and 'WithLogger'.
Taking into consideration all above
we get:

@
clientStart :: WithLoggerIO m => HostName -> .. -> m ClientComponentMap
clientStart hostName .. = do
    ...
    logName <- askLoggerName
    liftIO $ forkIO $ usingLoggerName logName $ routeIncoming endPoint msgs
    ...
@

So, here we see how useful this function can be.

@
clientStart :: WithLoggerIO m => HostName -> .. -> m ClientComponentMap
clientStart hostName .. = do
    ...
    liftLogIO forkIO $ routeIncoming endPoint msgs
    ...
@

-}
liftLogIO :: WithLoggerIO m => (IO a -> IO b) -> LoggerNameBox IO a -> m b
liftLogIO ioFunc action = do
    logName <- askLoggerName
    liftIO $ ioFunc $ usingLoggerName logName $ action
