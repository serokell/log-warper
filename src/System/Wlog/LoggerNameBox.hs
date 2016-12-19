{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | This module contains type classes for loggers that have 'LoggerName'.

module System.Wlog.LoggerNameBox
       ( -- * Remove boilerplater
         HasLoggerName (..)
       , LoggerNameBox (..)
       , setLoggerName
       , usingLoggerName
       ) where

import           Control.Lens                (iso)
import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Except        (ExceptT (..), runExceptT)
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Reader        (MonadReader (..), ReaderT, runReaderT)
import           Control.Monad.State         (MonadState (get), StateT, evalStateT)
import           Control.Monad.Trans         (MonadIO, MonadTrans, lift)
import           Control.Monad.Trans.Cont    (ContT, mapContT)
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           Control.Monad.Writer        (WriterT (..))

import           Serokell.Util.Lens          (WrappedM (..))

import           System.Wlog.LoggerName      (LoggerName)

-- | This type class exists to remove boilerplate logging
-- by adding the logger's name to the context in each module.
--
-- TODO: replace current methods with Lens?
class HasLoggerName m where
    -- | Extract logger name from context
    getLoggerName :: m LoggerName

    -- | Change logger name in context
    modifyLoggerName :: (LoggerName -> LoggerName) -> m a -> m a


instance (Monad m, HasLoggerName m) => HasLoggerName (ReaderT a m) where
    getLoggerName = lift getLoggerName

    modifyLoggerName how m =
        ask >>= lift . modifyLoggerName how . runReaderT m

instance (Monad m, HasLoggerName m) => HasLoggerName (StateT a m) where
    getLoggerName = lift getLoggerName

    modifyLoggerName how m =
        get >>= lift . modifyLoggerName how . evalStateT m

instance (Monoid w, Monad m, HasLoggerName m) => HasLoggerName (WriterT w m) where
    getLoggerName = lift getLoggerName

    modifyLoggerName how = WriterT . modifyLoggerName how . runWriterT

instance (Monad m, HasLoggerName m) => HasLoggerName (ExceptT e m) where
    getLoggerName = lift getLoggerName

    modifyLoggerName how = ExceptT . modifyLoggerName how . runExceptT

instance (Monad m, HasLoggerName m) => HasLoggerName (ContT r m) where
    getLoggerName = lift getLoggerName

    modifyLoggerName = mapContT . modifyLoggerName

-- | Set logger name in context.
setLoggerName :: HasLoggerName m => LoggerName -> m a -> m a
setLoggerName = modifyLoggerName . const

-- | Default implementation of `WithNamedLogger`.
newtype LoggerNameBox m a = LoggerNameBox
    { loggerNameBoxEntry :: ReaderT LoggerName m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadBase b,
                MonadThrow, MonadCatch, MonadMask, MonadState s, MonadFix)


instance MonadReader r m => MonadReader r (LoggerNameBox m) where
    ask = lift ask
    reader = lift . reader
    local f (LoggerNameBox m) = getLoggerName >>= lift . local f . runReaderT m

instance MonadBaseControl b m => MonadBaseControl b (LoggerNameBox m) where
    type StM (LoggerNameBox m) a = StM (ReaderT LoggerName m) a
    liftBaseWith io =
        LoggerNameBox $ liftBaseWith $ \runInBase -> io $ runInBase . loggerNameBoxEntry
    restoreM = LoggerNameBox . restoreM

instance Monad m => WrappedM (LoggerNameBox m) where
    type UnwrappedM (LoggerNameBox m) = ReaderT LoggerName m
    _WrappedM = iso loggerNameBoxEntry LoggerNameBox

-- | Runs a `LoggerNameBox` with specified initial `LoggerName`.
usingLoggerName :: LoggerName -> LoggerNameBox m a -> m a
usingLoggerName name = flip runReaderT name . loggerNameBoxEntry

instance Monad m => HasLoggerName (LoggerNameBox m) where
    getLoggerName = LoggerNameBox ask

    modifyLoggerName how = LoggerNameBox . local how . loggerNameBoxEntry
