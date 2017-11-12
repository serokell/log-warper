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

       -- * Logging functions
       , logDebug
       , logError
       , logInfo
       , logNotice
       , logWarning
       , logMessage
       ) where

import Universum

import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (MonadTrans (lift))

import System.Wlog.HasLoggerName (HasLoggerName (..))
import System.Wlog.IOLogger (logM)
import System.Wlog.LoggerName (LoggerName (..))
import System.Wlog.LoggerNameBox (LoggerNameBox (..))
import System.Wlog.Severity (Severity (..))

import qualified Control.Monad.RWS as RWSLazy
import qualified Control.Monad.RWS.Strict as RWSStrict
import qualified Control.Monad.State.Lazy as StateLazy


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
