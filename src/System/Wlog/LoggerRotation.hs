{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | This module contains type classes for loggers that rotate over filepaths

module System.Wlog.LoggerRotation
       ( IsRotatingLogger (..)
       , RollingLogger (..)
       ) where

import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Except        (ExceptT (..), mapExceptT)
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Reader        (MonadReader (..), ReaderT, mapReaderT)
import           Control.Monad.State         (MonadState, StateT, get, mapStateT, put,
                                              withStateT)
import           Control.Monad.Trans         (MonadIO, MonadTrans, lift)
import           Control.Monad.Trans.Cont    (ContT, mapContT)
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           Control.Monad.Writer        (WriterT (..), mapWriterT)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           System.FilePath             (FilePath)

import           System.Wlog.LoggerNameBox   (LoggerNameBox (..))

-- | This type class exists to allow loggers to cycle over filepaths as each gets full,
-- traversing a structure from left to right and going back to the oldest one after every
-- filepath has been filled.
class IsRotatingLogger m where
    -- | Rotate the logger's log location to avoid very long logs
    rotateLogger :: Text -> m a -> m a

logLimit :: Word
logLimit = 1024

type FileSize = Word

type FilesToRotate = [FilePath]

newtype RollingLogger m a = RollingLogger
    { getRotatingLogger :: StateT (FileSize, FilesToRotate) m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadBase b,
                MonadThrow, MonadCatch, MonadMask, MonadReader r, MonadFix)

instance IsRotatingLogger m => IsRotatingLogger (ReaderT a m) where
    rotateLogger = mapReaderT . rotateLogger

instance IsRotatingLogger m => IsRotatingLogger (StateT a m) where
    rotateLogger = mapStateT . rotateLogger

instance IsRotatingLogger m => IsRotatingLogger (WriterT a m) where
    rotateLogger = mapWriterT . rotateLogger

instance IsRotatingLogger m => IsRotatingLogger (ExceptT a m) where
    rotateLogger = mapExceptT . rotateLogger

instance IsRotatingLogger m => IsRotatingLogger (ContT a m) where
    rotateLogger = mapContT . rotateLogger

instance MonadState s m => MonadState s (RollingLogger m) where
    put = lift . put
    get = lift get

deriving instance IsRotatingLogger m => IsRotatingLogger (LoggerNameBox m)

-- Temporary
instance IsRotatingLogger IO where
    rotateLogger = flip const

instance MonadBaseControl b m => MonadBaseControl b (RollingLogger m) where
    type StM (RollingLogger m) a = StM (StateT (FileSize, FilesToRotate) m) a
    liftBaseWith io =
        RollingLogger $ liftBaseWith $ \runInBase -> io $ runInBase . getRotatingLogger
    restoreM = RollingLogger . restoreM

instance IsRotatingLogger (RollingLogger m) where
    rotateLogger leMessage = RollingLogger . withStateT rollIfLimit . getRotatingLogger
      where
        rollIfLimit (curFileSize, fps@(curLogFile : filePaths)) =
            let msgLen = fromIntegral $ T.length leMessage
                newLen = msgLen + curFileSize
            in if newLen > logLimit
                then (0, filePaths ++ [curLogFile])
                else (newLen, fps)
        rollIfLimit pair = pair
