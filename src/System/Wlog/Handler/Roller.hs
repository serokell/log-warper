-- | Custom implementation of 'LogHandler' with log rotation support.

module System.Wlog.Handler.Roller
       ( InvalidRotation (..)
       , RollerHandler   (..)
       , logIndex
       , rotationFileHandler
       ) where

import           Control.Concurrent         (modifyMVar, modifyMVar_, withMVar)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Formatting                 (sformat, shown, (%))
import           Universum

import           System.Directory           (removeFile, renameFile)
import           System.FilePath            ((<.>))
import           System.IO                  (Handle, IOMode (ReadWriteMode),
                                             SeekMode (AbsoluteSeek, SeekFromEnd), hClose,
                                             hFileSize, hFlush, hSeek)
import           System.Wlog.FileUtils      (whenExist)
import           System.Wlog.Formatter      (LogFormatter, nullFormatter)
import           System.Wlog.Handler        (LogHandler (..),
                                             LogHandlerTag (HandlerFilelike))
import           System.Wlog.Handler.Simple (GenericHandler (..), fileHandler)
import           System.Wlog.LoggerConfig   (RotationParameters (..), isValidRotation)
import           System.Wlog.Severity       (Severity (..))

-- | Similar to 'GenericHandler'. But holds file 'Handle' inside
-- mutable variable ('MVar') to be able to rotate loggers.
data RollerHandler = RollerHandler
    { rhSeverity    :: !Severity
    , rhFormatter   :: !(LogFormatter RollerHandler)
    , rhFileHandle  :: !(MVar Handle)
    , rhWriteAction :: !(Handle -> Text -> IO ())
    , rhCloseAction :: !(Handle -> IO ())
    , rhFileName    :: !FilePath
    }

instance LogHandler RollerHandler where
    getTag rh         = HandlerFilelike $ rhFileName rh
    setLevel     rh p = rh { rhSeverity  = p }
    setFormatter rh f = rh { rhFormatter = f }
    getLevel          = rhSeverity
    getFormatter      = rhFormatter
    readBack          = rollerReadback

    emit rh (_, msg) _      = rhWriteAction rh (error "Handler is used internally") msg
    close RollerHandler{..} = withMVar rhFileHandle rhCloseAction

data InvalidRotation = InvalidRotation !Text
    deriving (Show, Eq)

instance Exception InvalidRotation

logIndex :: FilePath -> Int -> FilePath
logIndex handlerPath i = handlerPath <.> show i

rollerReadback :: RollerHandler -> Int -> IO [Text]
rollerReadback RollerHandler{..} logsNum = do
    modifyMVar rhFileHandle $ \h -> do
        putText "Flush"
        hFlush h
        putText "Seek"
        hSeek h AbsoluteSeek 0
        putText "GetContents"
        contents <- T.lines <$> TIO.hGetContents h
        putText "Close"
        hClose h
        putText "Repoen"
        h' <- openFile rhFileName ReadWriteMode
        putText "Seek 2"
        hSeek h' SeekFromEnd 0
        pure (h', take logsNum $ reverse contents)

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
            h <- openFile handlerPath ReadWriteMode
            hSeek h SeekFromEnd 0
            pure h

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
