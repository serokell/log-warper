{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module deals with Exception logging.

module System.Wlog.Exception
       ( logException
       , catchLog
       ) where

import Universum

import System.Wlog.CanLog (WithLogger, WithLoggerIO, logError)

-- | Logs exception's description with ''System.Wlog.Severity.Error' 'System.Wlog.Severity.Severity'
logException :: forall e m . (WithLogger m, Exception e) => e -> m ()
logException = logError . show

{- | Runs the action, if an exception is raised the 'logException' is executed.

==== __Example__
Here is very simple example of usage 'catchLog' on IO functions:

@
main :: IO ()
main = do
    buildAndSetupYamlLogging productionB "log-config.yaml"
    usingLoggerName "new-logger" runWithExceptionLog

runWithExceptionLog :: (WithLoggerIO m, MonadCatch m) => m ()
runWithExceptionLog = catchLog @IOException (liftIO simpleIOfun)

simpleIOfun :: IO ()
simpleIOfun = getLine >>= readFile >>= putStrLn

@

and when run you will get:

>>> run-main-from-this-example
> not-existing-filename.txt
[new-logger:ERROR] [2017-12-01 13:07:33.21 UTC] asd: openFile: does not exist (No such file or directory)

-}
catchLog :: forall e m . (WithLoggerIO m, MonadCatch m, Exception e) => m () -> m ()
catchLog a = a `catch` logE
  where
    logE :: e -> m ()
    logE = logException
