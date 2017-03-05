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

import           Control.Concurrent         (MVar, modifyMVar_, newMVar)
import           Formatting                 (sformat, shown, (%))
import qualified Prelude                    (show)

import           System.Directory           (removeFile, renameFile)
import           System.FilePath            ((<.>))
import           System.IO                  (Handle, IOMode (AppendMode), hClose,
                                             hFileSize)
import           System.Wlog.FileUtils      (whenExist)
import           System.Wlog.Formatter      (LogFormatter, nullFormatter)
import           System.Wlog.Handler        (LogHandler (..))
import           System.Wlog.Handler.Simple (GenericHandler (..), fileHandler)
import           System.Wlog.LoggerConfig   (RotationParameters (..), isValidRotation)
import           System.Wlog.Severity       (Severity (..))

-- | Similar to 'GenericHandler'. But holds file 'Handle' inside
-- mutable variable ('MVar') to be able to rotate loggers.
data RollerHandler = RollerHandler
    { rhSeverity    :: Severity
    , rhFormatter   :: LogFormatter RollerHandler
    , rhFileHandle  :: MVar Handle
    , rhWriteAction :: Handle -> Text -> IO ()
    , rhCloseAction :: Handle -> IO ()
    , rhFileName    :: FilePath
    }

instance LogHandler RollerHandler where
    getTag rh         = "rollerHandler:" <> rhFileName rh
    setLevel     rh p = rh { rhSeverity  = p }
    setFormatter rh f = rh { rhFormatter = f }
    getLevel          = rhSeverity
    getFormatter      = rhFormatter

    emit rh (_, msg) _      = rhWriteAction rh (panic "Handler is used internally") msg
    close RollerHandler{..} = withMVar rhFileHandle rhCloseAction

data InvalidRotation = InvalidRotation !Text
    deriving (Show, Eq)

instance Exception InvalidRotation

logIndex :: FilePath -> Int -> FilePath
logIndex handlerPath i = handlerPath <.> Prelude.show i

-- TODO: correct exceptions handling here is too smart for me
rollerWriting
    :: RotationParameters
    -> FilePath
    -> (Handle -> Text -> IO ())
    -> MVar Handle
    -> Handle
    -> Text
    -> IO ()
rollerWriting RotationParameters{..} handlerPath loggingAction varHandle _ msg =
    modifyMVar_ varHandle $ \landle -> do
        loggingAction landle msg
        logFileSize <- fromIntegral <$> hFileSize landle
        if logFileSize < rpLogLimit
        then return landle
         -- otherwise should rename all files and create new handle putting in MVar
        else do
            hClose landle
            let lastIndex = fromIntegral $ rpKeepFiles - 1
            for_ [lastIndex - 1,lastIndex - 2 .. 0] $ \i -> do
                let oldLogFile = logIndex handlerPath i
                let newLogFile = logIndex handlerPath (i + 1)
                whenExist oldLogFile $ (`renameFile` newLogFile)
            let zeroIndex = logIndex handlerPath 0
            renameFile handlerPath zeroIndex
            let lastLogFile = logIndex handlerPath lastIndex
            whenExist lastLogFile removeFile
            openFile handlerPath AppendMode

-- | Create rotation logging handler.
rotationFileHandler
    :: MonadIO m
    => RotationParameters
    -> FilePath
    -> Severity
    -> m RollerHandler
rotationFileHandler rp@RotationParameters{..} _ _
    | not $ isValidRotation rp = liftIO $ throwM $ InvalidRotation $
      sformat ("Rotation parameters must be positive: "%shown) rp
rotationFileHandler rp@RotationParameters{..} handlerPath rhSeverity = liftIO $ do
    GenericHandler{..} <- fileHandler handlerPath rhSeverity
    rhFileHandle       <- newMVar privData
    let rhWriteAction   = rollerWriting rp handlerPath writeFunc rhFileHandle
    pure RollerHandler { rhFormatter   = nullFormatter
                       , rhCloseAction = closeFunc
                       , rhFileName = handlerPath
                       , ..
                       }
