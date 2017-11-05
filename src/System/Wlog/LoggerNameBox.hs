{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | This module contains type classes for loggers that have 'LoggerName'.

module System.Wlog.LoggerNameBox
       ( -- * Remove boilerplater
         LoggerNameBox (..)
       , usingLoggerName
       ) where

import           Universum

import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Except        (MonadError)
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Morph         (MFunctor (..))
import           Control.Monad.Reader        (MonadReader (..), ReaderT, runReaderT)
import           Control.Monad.State.Strict  (MonadState)
import           Control.Monad.Trans         (MonadIO, MonadTrans, lift)
import           Control.Monad.Trans.Control (MonadBaseControl (..))

import           System.Wlog.HasLoggerName   (HasLoggerName (..))
import           System.Wlog.LoggerName      (LoggerName)

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
