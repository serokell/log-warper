{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Contains base monad for all logging actions in @log-warper@ library. This
-- monad keeps logging 'Context' inside 'ReaderT'.

module Log.Monad
       ( -- * Logger Context
         Context (..)
       , ctxName
       , ctxConfiguration

         -- * Logger monads
       , LoggerT (..)
       , MonadLogContext (..)
       , WarperAction
       , WithLogger
       , launchLogger

         -- * Logging functions
       , logM
       , withSublogger
       ) where

import Universum

import Lens.Micro.Platform (makeLenses)

import Log.Configuration (Configuration, Extension)
import Log.Core.Action (LogAction (..))
import Log.Core.Class (ActionT, MonadLogger, logR, usingActionT, withR)
import Log.Event (Event (..), eName)
import Log.Name (LoggerName)
import Log.Severity (Severity)

-- | Runtime context for main 'LoggerT' monad. Keeps current 'LoggerName' inside
-- 'ReaderT' context as well as global configuration.
data Context (exts :: [Extension]) = Context
    { _ctxName          :: !LoggerName
    , _ctxConfiguration :: !(Configuration exts)
    }

makeLenses ''Context

-- | Logging monad with access to 'Context'.
newtype LoggerT exts m a = LoggerT
    { runLoggerT :: ReaderT (Context exts) m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

-- | Type for all logging actions which are supposed to be used in @log-warper@.
type WarperAction exts m = LogAction (LoggerT exts m) (Event exts)

{- | Type class for monads which have access to 'Context'.

__Laws:__

* __@Idempotency:@__ context is not changed over time

@
askLogContext >> askLogContext ≡ askLogContext
@

Strictly speaking, this law doesn't hold for 'LoggerT' when somebody calls
'Log.Core.Class.withLogAction' function but this should be ok.
-}
class Monad m => MonadLogContext m exts | m -> exts where
    -- | Ask 'Context' from monadic context
    askLogContext :: m (Context exts)

    default askLogContext :: (MonadTrans t, t n ~ m, MonadLogContext n exts)
                          => m (Context exts)
    askLogContext = lift askLogContext

instance MonadLogContext m exts => MonadLogContext (ExceptT e m) exts
instance MonadLogContext m exts => MonadLogContext (ReaderT e m) exts
instance MonadLogContext m exts => MonadLogContext (StateT  s m) exts
instance MonadLogContext m exts => MonadLogContext (ActionT r m) exts

instance Monad m => MonadLogContext (LoggerT exts m) exts where
    askLogContext :: LoggerT exts m (Context exts)
    askLogContext = LoggerT ask

{- | Convenient type alias for monad which can log @'Event' exts@. To make
logging functions nicer (i.e. to get current logger name and configuration from
context) we also need extra 'MonadLogContext' constraint.
-}
type WithLogger m exts = (MonadLogger m (Event exts), MonadLogContext m exts)

-- TODO: rename to just @log@ after universum stops reexporting @log@ function
-- | Logs message by given severity and logger message.
logM :: WithLogger m exts => Severity -> Text -> m ()
logM severity msg = do
    Context name cfg <- askLogContext
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
withSublogger :: MonadLogger m (Event exts) => LoggerName -> m a -> m a
withSublogger child = withR (eName %~ (<> child))
-- TODO: replace with <>~ after in microlens

-- TODO: add big example
-- | Launches logger monad.
launchLogger :: WarperAction exts m  -- ^ Logging action
             -> Context exts  -- ^ Context with current logger name and global paramters
             -> ActionT (Event exts) (LoggerT exts m) a  -- ^ Monadic action
             -> m a
launchLogger action context = usingReaderT context . runLoggerT . usingActionT action
