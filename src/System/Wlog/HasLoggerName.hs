{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | This module contains type class for 'HasLoggerName'.

module System.Wlog.HasLoggerName
       ( -- * Remove boilerplater
         HasLoggerName (..)
       , setLoggerName
       ) where

import           Universum

import           Control.Monad.Except       (ExceptT (..))
import           Control.Monad.Morph        (MFunctor (..))
import qualified Control.Monad.RWS          as RWSLazy
import qualified Control.Monad.RWS.Strict   as RWSStrict
import qualified Control.Monad.State        as StateLazy (StateT)
import           Control.Monad.State.Strict (StateT)
import           Control.Monad.Trans        (MonadTrans, lift)
import           Control.Monad.Trans.Cont   (ContT, mapContT)
import           Control.Monad.Writer       (WriterT (..))

import           System.Wlog.LoggerName     (LoggerName)

-- | This type class exists to remove boilerplate logging
-- by adding the logger's name to the context in each module.
--
-- TODO: replace current methods with Lens?
class HasLoggerName m where
    -- | Extract logger name from context
    askLoggerName :: m LoggerName

    -- | Change logger name in context
    modifyLoggerName :: (LoggerName -> LoggerName) -> m a -> m a

    default askLoggerName :: (MonadTrans t, t n ~ m, Monad n, HasLoggerName n) => m LoggerName
    askLoggerName = lift askLoggerName

    default modifyLoggerName :: (MFunctor t, t n ~ m, Monad n, HasLoggerName n)
                             => (LoggerName -> LoggerName)
                             -> m a
                             -> m a
    modifyLoggerName f = hoist (modifyLoggerName f)

instance (Monad m, HasLoggerName m) => HasLoggerName (ReaderT a m) where
instance (Monad m, HasLoggerName m) => HasLoggerName (StateT a m) where
instance (Monad m, HasLoggerName m) => HasLoggerName (StateLazy.StateT a m) where
instance (Monoid w, Monad m, HasLoggerName m) => HasLoggerName (WriterT w m) where
instance (Monad m, HasLoggerName m) => HasLoggerName (ExceptT e m) where
instance (Monad m, HasLoggerName m) => HasLoggerName (ContT r m) where
    askLoggerName    = lift askLoggerName
    modifyLoggerName = mapContT . modifyLoggerName

instance (Monad m, HasLoggerName m, Monoid w) => HasLoggerName (RWSLazy.RWST r w s m) where
instance (Monad m, HasLoggerName m, Monoid w) => HasLoggerName (RWSStrict.RWST r w s m) where

instance HasLoggerName Identity where
    askLoggerName    = Identity "Identity"
    modifyLoggerName = flip const

-- | Set logger name in context.
setLoggerName :: HasLoggerName m => LoggerName -> m a -> m a
setLoggerName = modifyLoggerName . const
