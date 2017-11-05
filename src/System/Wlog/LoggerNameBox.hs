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

import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Except        (ExceptT (..), mapExceptT)
import           Control.Monad.Except        (MonadError)
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Morph         (MFunctor (..))
import           Control.Monad.Reader        (MonadReader (..), ReaderT, mapReaderT,
                                              runReaderT)
import qualified Control.Monad.RWS           as RWSLazy
import qualified Control.Monad.RWS.Strict    as RWSStrict
import qualified Control.Monad.State         as StateLazy (StateT, mapStateT)
import           Control.Monad.State.Strict  (MonadState, StateT, mapStateT)
import           Control.Monad.Trans         (MonadIO, MonadTrans, lift)
import           Control.Monad.Trans.Cont    (ContT, mapContT)
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           Control.Monad.Writer        (WriterT (..), mapWriterT)
import           Universum

import           System.Wlog.LoggerName      (LoggerName)

-- | This type class exists to remove boilerplate logging
-- by adding the logger's name to the context in each module.
--
-- TODO: replace current methods with Lens?
class HasLoggerName m where
    -- | Extract logger name from context
    askLoggerName :: m LoggerName

    -- | Change logger name in context
    modifyLoggerName :: (LoggerName -> LoggerName) -> m a -> m a

instance (Monad m, HasLoggerName m) => HasLoggerName (ReaderT a m) where
    askLoggerName = lift askLoggerName

    modifyLoggerName = mapReaderT . modifyLoggerName

instance (Monad m, HasLoggerName m) => HasLoggerName (StateT a m) where
    askLoggerName = lift askLoggerName

    modifyLoggerName = mapStateT . modifyLoggerName

instance (Monad m, HasLoggerName m) => HasLoggerName (StateLazy.StateT a m) where
    askLoggerName = lift askLoggerName
    modifyLoggerName = StateLazy.mapStateT . modifyLoggerName

instance (Monoid w, Monad m, HasLoggerName m) => HasLoggerName (WriterT w m) where
    askLoggerName = lift askLoggerName

    modifyLoggerName = mapWriterT . modifyLoggerName

instance (Monad m, HasLoggerName m) => HasLoggerName (ExceptT e m) where
    askLoggerName = lift askLoggerName

    modifyLoggerName = mapExceptT . modifyLoggerName

instance (Monad m, HasLoggerName m) => HasLoggerName (ContT r m) where
    askLoggerName = lift askLoggerName

    modifyLoggerName = mapContT . modifyLoggerName

instance (Monad m, HasLoggerName m, Monoid w) => HasLoggerName (RWSLazy.RWST r w s m) where
    askLoggerName = lift askLoggerName

    modifyLoggerName = RWSLazy.mapRWST . modifyLoggerName

instance (Monad m, HasLoggerName m, Monoid w) => HasLoggerName (RWSStrict.RWST r w s m) where
    askLoggerName = lift askLoggerName

    modifyLoggerName = RWSStrict.mapRWST . modifyLoggerName

instance HasLoggerName Identity where
    askLoggerName    = Identity "Identity"
    modifyLoggerName = flip const

-- | Set logger name in context.
setLoggerName :: HasLoggerName m => LoggerName -> m a -> m a
setLoggerName = modifyLoggerName . const

-- | Default implementation of `WithNamedLogger`.
newtype LoggerNameBox m a = LoggerNameBox
    { loggerNameBoxEntry :: ReaderT LoggerName m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadBase b,
                MonadThrow, MonadCatch, MonadMask, MonadError e, MonadState s,
                MonadFix)

instance MonadReader r m => MonadReader r (LoggerNameBox m) where
    ask = lift ask
    reader = lift . reader
    local f (LoggerNameBox m) = askLoggerName >>= lift . local f . runReaderT m

instance MonadBaseControl b m => MonadBaseControl b (LoggerNameBox m) where
    type StM (LoggerNameBox m) a = StM (ReaderT LoggerName m) a
    liftBaseWith io =
        LoggerNameBox $ liftBaseWith $ \runInBase -> io $ runInBase . loggerNameBoxEntry
    restoreM = LoggerNameBox . restoreM

instance MFunctor LoggerNameBox where
    hoist f = LoggerNameBox . hoist f . loggerNameBoxEntry

-- | Runs a `LoggerNameBox` with specified initial `LoggerName`.
usingLoggerName :: LoggerName -> LoggerNameBox m a -> m a
usingLoggerName name = flip runReaderT name . loggerNameBoxEntry

instance Monad m => HasLoggerName (LoggerNameBox m) where
    askLoggerName = LoggerNameBox ask

    modifyLoggerName how = LoggerNameBox . local how . loggerNameBoxEntry
