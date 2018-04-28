{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Monad type class for things that have logging ability.

module Logw.Core.Class
       ( -- * Interface
         MonadLogger (..)
       , logR
       , withR
       , askLogActionLifted
       , unliftLogAction

         -- * Basic interface implementation
       , LoggerT (..)
       , usingLoggerT
       ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (ExceptT, mapExceptT, runExceptT)
import Control.Monad.Trans.Identity (IdentityT (..), mapIdentityT)
import Control.Monad.Trans.Maybe (MaybeT (..), mapMaybeT)
import Control.Monad.Trans.Reader (ReaderT (..), ask, mapReaderT)
import Control.Monad.Trans.State.Strict (StateT (..), get, mapStateT)
import Data.Kind (Type)

import Logw.Core.Action (LogAction (..), cmap)

{- | Type class for monads that have logging ability. In order to log things
monad @m@ should be able to construct 'LogAction' for some logging message @msg@.

__Laws:__

Some laws duplicate @lens@ laws. And maybe some of the laws are redundant and
can be proven from other laws.

* __@Identity:@__ 'id' function shouldn't change anything:

@
'withLogAction' id ≡ id
@

* __@Composition:@__ composition of two 'withLogAction' is a single
  'withLogAction' with function composition:

@
'withLogAction' f . 'withLogAction' g ≡ 'withLogAction' (f . g)
@

* __@GetGet:@__ @askLogAction@ shouldn't mutate monadic context:

@
'askLogAction' >> 'askLogAction' ≡ 'askLogAction'
@

* __@PutGet:@__ 'askLogAction' after 'withLogAction' should return modified action.

@
'withLogAction' f 'askLogAction' ≡ 'fmap' f 'askLogAction'
@

* __@GetPut:@__ 'withLogAction' which puts result 'askLogAction' shouldn't change anything.

@
'askLogAction' >>= \\action -> 'withLogAction' ('const' action) m ≡ m
@
-}
class Monad m => MonadLogger (msg :: Type) (m :: Type -> Type) | m -> msg where
    {-# MINIMAL askLogAction, withLogAction #-}

    -- | Return 'LogAction' from current monadic context. You can use
    -- 'askLogActionLifted' function to implement this method for monad transformer.
    askLogAction :: m (LogAction m msg)

    -- | Runs given monadic computation with changed 'LogAction'.
    withLogAction :: (LogAction m msg -> LogAction m msg) -> m a -> m a

-- | Default implementation of 'askLogAction' for monad transformer.
askLogActionLifted :: (MonadTrans t, t n ~ m, MonadLogger msg m, MonadLogger msg n) => m (LogAction m msg)
askLogActionLifted = do
    LogAction action <- lift askLogAction
    pure $ LogAction (lift . action)

-- | Helper function. Helps to create instance of 'MonadLogger', especially to
-- implement 'withLogAction' method.
unliftLogAction :: (MonadTrans t, Monad m)
                => (t m () -> m a)  -- ^ Transformer unwrapper for logging action
                -> (LogAction (t m) msg -> LogAction (t m) msg)
                -> LogAction m msg
                -> LogAction m msg
unliftLogAction unlift f (LogAction action) = case f $ LogAction (lift . action) of
    LogAction transAction -> LogAction (void . unlift . transAction)

instance MonadLogger msg m => MonadLogger msg (IdentityT m) where
    askLogAction :: IdentityT m (LogAction (IdentityT m) msg)
    askLogAction = askLogActionLifted

    withLogAction :: (LogAction (IdentityT m) msg -> LogAction (IdentityT m) msg) -> IdentityT m a -> IdentityT m a
    withLogAction = mapIdentityT . withLogAction . unliftLogAction runIdentityT

-- | __TODO:__ Not sure about this instance; it might contain error
instance MonadLogger msg m => MonadLogger msg (MaybeT m) where
    askLogAction :: MaybeT m (LogAction (MaybeT m) msg)
    askLogAction = askLogActionLifted

    withLogAction :: (LogAction (MaybeT m) msg -> LogAction (MaybeT m) msg) -> MaybeT m a -> MaybeT m a
    withLogAction = mapMaybeT . withLogAction . unliftLogAction runMaybeT

-- | __TODO:__ Not sure about this instance; it might contain error
instance MonadLogger msg m => MonadLogger msg (ExceptT e m) where
    askLogAction :: ExceptT e m (LogAction (ExceptT e m) msg)
    askLogAction = askLogActionLifted

    withLogAction :: (LogAction (ExceptT e m) msg -> LogAction (ExceptT e m) msg) -> ExceptT e m a -> ExceptT e m a
    withLogAction = mapExceptT . withLogAction . unliftLogAction runExceptT

instance MonadLogger msg m => MonadLogger msg (ReaderT e m) where
    askLogAction :: ReaderT e m (LogAction (ReaderT e m) msg)
    askLogAction = askLogActionLifted

    withLogAction :: (LogAction (ReaderT e m) msg -> LogAction (ReaderT e m) msg) -> ReaderT e m a -> ReaderT e m a
    withLogAction f action = do
        env <- ask
        mapReaderT (withLogAction (unliftLogAction (flip runReaderT env) f)) action

-- | __TODO:__ Not sure about this instance; it might contain error
instance MonadLogger msg m => MonadLogger msg (StateT s m) where
    askLogAction :: StateT s m (LogAction (StateT s m) msg)
    askLogAction = askLogActionLifted

    withLogAction :: (LogAction (StateT s m) msg -> LogAction (StateT s m) msg) -> StateT s m a -> StateT s m a
    withLogAction f action = do
        st <- get
        mapStateT (withLogAction (unliftLogAction (flip runStateT st) f)) action

{- | Perform logging action with given logging record @r@. You can use this
function like this:

@
__foo__ :: 'MonadLogger' 'String' m => m ()
__foo__ = __do__
    'logR' "Some logging"
    'logR' "Another logging"
@
-}
logR :: MonadLogger msg m => msg -> m ()
logR msg = do
    LogAction action <- askLogAction
    action msg

{- | Executes given monadic logging action by applying function to every logging record.

@
__app__ :: 'MonadLogger' 'String' m => m ()
__app__ = 'withR' ("app:" ++) $ __do__
    'logR' "Some logging"
    'logR' "Another logging"
@
-}
withR :: MonadLogger msg m => (msg -> msg) -> m a -> m a
withR = withLogAction . cmap

{- | Simple implementation for 'MonadLogger' type class. This data type just
stores 'LogAction' which performs logging action under monad @m@.
-}
newtype LoggerT msg m a = LoggerT
    { runLoggerT :: ReaderT (LogAction m msg) m a
    } deriving (Functor, Applicative, Monad, MonadIO)

-- TODO: depend on mtl and implement MonadError, etc?..

instance MonadTrans (LoggerT msg) where
    lift :: Monad m => m a -> LoggerT msg m a
    lift = LoggerT . lift

instance Monad m => MonadLogger msg (LoggerT msg m) where
    askLogAction :: LoggerT msg m (LogAction (LoggerT msg m) msg)
    askLogAction = LoggerT $ do
        LogAction action <- ask
        pure $ LogAction $ lift . action

    -- this implementation is crazy, I hope it's correct...
    withLogAction :: (LogAction (LoggerT msg m) msg -> LogAction (LoggerT msg m) msg)
                  -> LoggerT msg m a
                  -> LoggerT msg m a
    withLogAction f m = LoggerT $ do
        logAction@(LogAction action) <- ask
        let transLogAction = LogAction (lift . action)
        let LogAction newAction = f transLogAction
        let unliftAction = LogAction (usingLoggerT logAction . newAction)
        lift $ usingLoggerT unliftAction m

{- | Runner for 'LoggerT' monad. Let's consider one simple example of monadic
action you have:

@
__example__ :: 'MonadLogger' 'String' m => m ()
__example__ = __do__
    'logR' "Starting application..."
    'withR' ("app:" ++) $ __do__
        'logR' "Application started."
        'logR' "Application finished."
@

You can use the following way of running such example:

@
'usingLoggerT' ('LogAction' 'putStrLn') example
@

And you will see this output:

@
Starting application...
app:Application started.
app:Application finished.
@
-}
usingLoggerT :: LogAction m msg -> LoggerT msg m a -> m a
usingLoggerT action =  flip runReaderT action . runLoggerT
