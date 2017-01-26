{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Custom implementation of 'LogHandler' with log rotation support.

module System.Wlog.Roller
       ( InvalidRotation (..)
       , RollerHandler   (..)
       , logIndex
       , rotationFileHandler
       ) where

import           Universum

import           Control.Concurrent        (MVar, modifyMVar_, newMVar)
import           Formatting                (sformat, shown, (%))
import qualified Prelude                   (show)

import           System.Directory          (removeFile, renameFile)
import           System.FilePath           ((<.>))
import           System.IO                 (Handle, IOMode (AppendMode), hClose,
                                            hFileSize)
import           System.Log                (Priority)
import           System.Log.Formatter      (LogFormatter, nullFormatter)
import           System.Log.Handler        (LogHandler (..))
import           System.Log.Handler.Simple (GenericHandler (..), fileHandler)

import           System.Wlog.FileUtils     (whenExist)
import           System.Wlog.LoggerConfig  (RotationParameters (..), isValidRotation)

-- | Similar to 'GenericHandler'. But holds file 'Handle' inside
-- mutable variable ('MVar') to be able to rotate loggers.
data RollerHandler = RollerHandler
    { rhPriority    :: Priority
    , rhFormatter   :: LogFormatter RollerHandler
    , rhFileHandle  :: MVar Handle
    , rhWriteAction :: Handle -> String -> IO ()
    , rhCloseAction :: Handle -> IO ()
    }

instance LogHandler RollerHandler where
    setLevel     rh p = rh { rhPriority  = p }
    setFormatter rh f = rh { rhFormatter = f }
    getLevel          = rhPriority
    getFormatter      = rhFormatter

    emit rh (_, msg) _      = rhWriteAction rh (panic "Handler is used internally") msg
    close RollerHandler{..} = withMVar rhFileHandle rhCloseAction

data InvalidRotation = InvalidRotation !Text
    deriving (Show, Eq)

instance Exception InvalidRotation

logIndex :: FilePath -> Word -> FilePath
logIndex handlerPath i = handlerPath <.> Prelude.show i

-- | Create rotation logging handler.
rotationFileHandler
    :: MonadIO m
    => RotationParameters
    -> FilePath
    -> Priority
    -> m RollerHandler
rotationFileHandler rp@RotationParameters{..} _ _
    | not $ isValidRotation rp = liftIO $ throwM $ InvalidRotation $
      sformat ("Rotation parameters must be positive: "%shown) rp
rotationFileHandler RotationParameters{..} handlerPath rhPriority = liftIO $ do
    GenericHandler{..} <- fileHandler handlerPath rhPriority
    rhFileHandle       <- newMVar privData
    let rhWriteAction   = rollerWriting writeFunc rhFileHandle
    return RollerHandler{ rhFormatter   = nullFormatter
                        , rhCloseAction = closeFunc
                        , ..
                        }
  where
    -- TODO: correct exceptions handling here is too smart for me
    rollerWriting
        :: (Handle -> String -> IO ())
        -> MVar Handle
        -> Handle
        -> String
        -> IO ()
    rollerWriting loggingAction varHandle _ msg = modifyMVar_ varHandle $ \landle -> do
        loggingAction landle msg
        logFileSize <- fromIntegral <$> hFileSize landle
        if logFileSize < rpLogLimit then
            return landle
        else do  -- otherwise should rename all files and create new handle putting in MVar
           hClose landle
           let lastIndex = rpKeepFiles - 1

           for_ [lastIndex - 1, lastIndex - 2 .. 0] $ \i -> do
               let oldLogFile = logIndex handlerPath i
               let newLogFile = logIndex handlerPath (i + 1)
               whenExist oldLogFile $ (`renameFile` newLogFile)

           let zeroIndex = logIndex handlerPath 0
           renameFile handlerPath zeroIndex

           let lastLogFile = logIndex handlerPath lastIndex
           whenExist lastLogFile removeFile
           openFile handlerPath AppendMode
