{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Contains base monad for all logging actions in @log-warper@ library. This
-- monad keeps logging 'LogCtx' inside 'ReaderT'.

module Logw.Monad
       ( -- * Logger Context
         LogCtx (..)
       , ctxName
       , ctxConfiguration

         -- * Logger monads
       , ContextT (..)
       , MonadLogContext (..)
       , WithLogger
       , launchLogger

         -- * Logging functions
       , logM
       , withSublogger
       ) where

import Universum

import Lens.Micro.Platform (makeLenses)

import Logw.Configuration (Configuration, Extension)
import Logw.Core.Action (LogAction (..))
import Logw.Core.Class (LoggerT, MonadLogger, logR, usingLoggerT, withR)
import Logw.Event (Event (..), eName)
import Logw.Name (LoggerName)
import Logw.Severity (Severity)

-- | Runtime context for main 'LoggerT' monad. Keeps current 'LoggerName' inside
-- 'ReaderT' context as well as global configuration.
data LogCtx (exts :: [Extension]) = LogCtx
    { _ctxName          :: !LoggerName
    , _ctxConfiguration :: !(Configuration exts)
    }

makeLenses ''LogCtx

-- | Logging monad with access to 'LogCtx'.
newtype ContextT exts m a = ContextT
    { runContextT :: ReaderT (LogCtx exts) m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

{- | Type class for monads which have access to 'Context'.

__Laws:__

* __@GetGet:@

@
askLogContext >> askLogContext ≡ askLogContext
@
-}
class Monad m => MonadLogContext exts m | m -> exts where
    -- | Ask 'Context' from monadic context
    askLogContext :: m (LogCtx exts)

    default askLogContext :: (MonadTrans t, t n ~ m, MonadLogContext exts n)
                          => m (LogCtx exts)
    askLogContext = lift askLogContext

instance MonadLogContext exts m => MonadLogContext exts (ExceptT e m)
instance MonadLogContext exts m => MonadLogContext exts (ReaderT e m)
instance MonadLogContext exts m => MonadLogContext exts (StateT  s m)
instance MonadLogContext exts m => MonadLogContext exts (LoggerT r m)

instance Monad m => MonadLogContext exts (ContextT exts m) where
    askLogContext :: ContextT exts m (LogCtx exts)
    askLogContext = ContextT ask

{- | Convenient type alias for monad which can log @'Event' exts@. To make
logging functions nicer (i.e. to get current logger name and configuration from
context) we also need extra 'MonadLogContext' constraint.
-}
type WithLogger exts m = (MonadLogger (Event exts) m, MonadLogContext exts m)

-- TODO: rename to just @log@ after universum stops reexporting @log@ function
-- | Logs message by given severity and logger message.
logM :: WithLogger exts m => Severity -> Text -> m ()
logM severity msg = do
    LogCtx name cfg <- askLogContext
    logR $ Event name severity msg cfg

{- | Runs given action with given 'LoggerName' added as child to existing log.

@
example :: WithLogger m exts => m ()
example = do
    logM Debug "Starting node..."  -- logs with given logger name "foo"
    withSublogger "node" $
        logM Info "Node started."  -- logs with logger "foo.node"
@
-}
withSublogger :: MonadLogger (Event exts) m => LoggerName -> m a -> m a
withSublogger child = withR (eName %~ (<> child))
-- TODO: replace with <>~ after in microlens

-- TODO: add big example
-- | Launches logger monad.
launchLogger :: LogAction (ContextT exts m) (Event exts)  -- ^ Logging action
             -> LogCtx exts  -- ^ Context with current logger name and global paramters
             -> LoggerT (Event exts) (ContextT exts m) a  -- ^ Monadic action
             -> m a
launchLogger action context = usingReaderT context . runContextT . usingLoggerT action
