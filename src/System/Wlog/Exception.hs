{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module deals with Exception logging.

module System.Wlog.Exception
       ( logException
       , catchLog
       ) where

import Universum

import Control.Exception.Base (Exception)

import System.Wlog.CanLog (WithLogger, WithLoggerIO, logError)

-- | Logs exception's description with ''System.Wlog.Severity.Error' 'System.Wlog.Severity.Severity'
logException :: (WithLogger m, Exception e) => e -> m ()
logException e = logError $ show e

-- | Runs the action, if an exception is raised the 'logException' is executed.
catchLog :: forall m e . (WithLoggerIO m, MonadCatch m, Exception e) => m () -> m ()
catchLog a = a `catch` logE
  where
    logE :: e -> m ()
    logE = logException
