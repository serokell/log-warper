{-# LANGUAGE DefaultSignatures          #-}
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

module Log.Core.Class
       ( -- * Interface
         MonadLogger (..)
       , logR
       , withR
       , unliftLogAction

         -- * Basic interface implementation
       , ActionT (..)
       , usingActionT
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

import Log.Core.Action (LogAction (..), cmap)

{- | Type class for monads that have logging ability. In order to log things
monad @m@ should be able to construct 'LogAction' for some logging record @r@.

__Laws:__

Some laws duplicate @lens@ laws. And maybe of the laws are redundant and can be
proven from other laws.

* __@Idempotency:@__ @askLogAction@ shouldn't mutate monadic context:

@
'askLogAction' >> 'askLogAction' ≡ 'askLogAction'
@

* __@Identity:@__ 'id' function shouldn't change anything:

@
'withLogAction' id ≡ id
@

* __@Composition:@__ composition of two 'withLogAction' is a single
  'withLogAction' with function composition:

@
'withLogAction' f . 'withLogAction' g ≡ 'withLogAction' (f . g)
@

* __@PutGet:@__ 'askLogAction' after 'withLogAction' should return modified action.

@
'withLogAction' f 'askLogAction' ≡ 'fmap' f 'askLogAction'
@
-}
class Monad m => MonadLogger (m :: Type -> Type) (r :: Type) | m -> r where
    -- | Return 'LogAction' from current monadic context.
    askLogAction :: m (LogAction m r)

    default askLogAction :: (MonadTrans t, t n ~ m, MonadLogger n r)
                         => m (LogAction m r)
    askLogAction = do
        LogAction action <- lift askLogAction
        pure $ LogAction (lift . action)

    -- | Runs given monadic computation with changed 'LogAction'.
    withLogAction :: (LogAction m r -> LogAction m r) -> m a -> m a

-- | Helper function. Helps to create instance of 'MonadLogger', especially to
-- implement 'withLogAction' method.
unliftLogAction :: (MonadTrans t, Monad m)
                => (t m () -> m a)  -- ^ Transformer unwrapper for logging action
                -> (LogAction (t m) r -> LogAction (t m) r)
                -> LogAction m r
                -> LogAction m r
unliftLogAction unlift f (LogAction action) = case f $ LogAction (lift . action) of
    LogAction transAction -> LogAction (void . unlift . transAction)

instance MonadLogger m r => MonadLogger (IdentityT m) r where
    withLogAction :: (LogAction (IdentityT m) r -> LogAction (IdentityT m) r) -> IdentityT m a -> IdentityT m a
    withLogAction = mapIdentityT . withLogAction . unliftLogAction runIdentityT

instance MonadLogger m r => MonadLogger (MaybeT m) r where
    withLogAction :: (LogAction (MaybeT m) r -> LogAction (MaybeT m) r) -> MaybeT m a -> MaybeT m a
    withLogAction = mapMaybeT . withLogAction . unliftLogAction runMaybeT

instance MonadLogger m r => MonadLogger (ExceptT e m) r where
    withLogAction :: (LogAction (ExceptT e m) r -> LogAction (ExceptT e m) r) -> ExceptT e m a -> ExceptT e m a
    withLogAction = mapExceptT . withLogAction . unliftLogAction runExceptT

instance MonadLogger m r => MonadLogger (ReaderT e m) r where
    withLogAction :: (LogAction (ReaderT e m) r -> LogAction (ReaderT e m) r) -> ReaderT e m a -> ReaderT e m a
    withLogAction f action = do
        env <- ask
        mapReaderT (withLogAction (unliftLogAction (flip runReaderT env) f)) action

-- | __TODO:__ Not sure about this instance; it might contain error
instance MonadLogger m r => MonadLogger (StateT s m) r where
    withLogAction :: (LogAction (StateT s m) r -> LogAction (StateT s m) r) -> StateT s m a -> StateT s m a
    withLogAction f action = do
        st <- get
        mapStateT (withLogAction (unliftLogAction (flip runStateT st) f)) action

{- | Perform logging action with given logging record @r@. You can use this
function like this:

@
__foo__ :: 'MonadLogger' m 'String' => m ()
__foo__ = __do__
    'logR' "Some logging"
    'logR' "Another logging"
@
-}
logR :: MonadLogger m r => r -> m ()
logR r = do
    LogAction action <- askLogAction
    action r

{- | Executes given monadic logging action by applying function to every logging record.

@
__app__ :: 'MonadLogger' m 'String' => m ()
__app__ = 'withR' ("app:" ++) $ __do__
    'logR' "Some logging"
    'logR' "Another logging"
@
-}
withR :: MonadLogger m r => (r -> r) -> m a -> m a
withR = withLogAction . cmap

{- | Simple implementation for 'MonadLogger' type class. This data type just
stores 'LogAction' which performs logging action under monad @m@.
-}
newtype ActionT r m a = ActionT
    { runActionT :: ReaderT (LogAction m r) m a
    } deriving (Functor, Applicative, Monad, MonadIO)

-- TODO: depend on mtl and implement MonadError and etc?..

instance MonadTrans (ActionT r) where
    lift :: Monad m => m a -> ActionT r m a
    lift = ActionT . lift

instance Monad m => MonadLogger (ActionT r m) r where
    askLogAction :: ActionT r m (LogAction (ActionT r m) r)
    askLogAction = ActionT $ do
        LogAction action <- ask
        pure $ LogAction $ lift . action

    -- this implementation is crazy, I hope it's correct...
    withLogAction :: (LogAction (ActionT r m) r -> LogAction (ActionT r m) r) -> ActionT r m a -> ActionT r m a
    withLogAction f m = ActionT $ do
        logAction@(LogAction action) <- ask
        let transLogAction = LogAction (lift . action)
        let LogAction newAction = f transLogAction
        let unliftAction = LogAction (usingActionT logAction . newAction)
        lift $ usingActionT unliftAction m

{- | Runner for 'ActionT' monad. Let's consider one simple example of monadic
action you have:

@
__example__ :: 'MonadLogger' m 'String' => m ()
__example__ = __do__
    'logR' "Starting application..."
    'withR' ("app:" ++) $ __do__
        'logR' "Application started."
        'logR' "Application finished."
@

You can use the following way of running such example:

@
'usingActionT' ('LogAction' 'putStrLn') example
@

And you will see this output:

@
Starting application...
app:Application started.
app:Application finished.
@
-}
usingActionT :: LogAction m r -> ActionT r m a -> m a
usingActionT action =  flip runReaderT action . runActionT
