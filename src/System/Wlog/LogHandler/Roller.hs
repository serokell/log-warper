-- | Custom implementation of 'LogHandler' with log rotation support.

module System.Wlog.LogHandler.Roller
       ( InvalidRotation (..)
       , RollerHandler   (..)
       , logIndex
       , rotationFileHandler
       ) where

import Universum

import Control.Concurrent (modifyMVar, modifyMVar_, withMVar)
import Data.Text.Lazy.Builder as B
import Fmt ((+||), (||+))

import System.Directory (removeFile, renameFile)
import System.FilePath ((<.>))
import System.IO (SeekMode (AbsoluteSeek, SeekFromEnd), hFileSize,
                  hFlush, hSeek)

import System.Wlog.FileUtils (whenExist)
import System.Wlog.Formatter (LogFormatter, nullFormatter)
import System.Wlog.LoggerConfig (RotationParameters (..), isValidRotation)
import System.Wlog.LogHandler (LogHandler (..), LogHandlerTag (HandlerFilelike))
import System.Wlog.LogHandler.Simple (GenericHandler (..), fileHandler)
import System.Wlog.Severity (Severities)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | Similar to 'GenericHandler'. But holds file 'Handle' inside
-- mutable variable ('MVar') to be able to rotate loggers.
data RollerHandler = RollerHandler
    { rhSeverities  :: !Severities
    , rhFormatter   :: !(LogFormatter RollerHandler)
    , rhFileHandle  :: !(MVar Handle)
    , rhWriteAction :: !(Handle -> Text -> IO ())
    , rhCloseAction :: !(Handle -> IO ())
    , rhFileName    :: !FilePath
    }

instance LogHandler RollerHandler where
    getTag rh         = HandlerFilelike $ rhFileName rh
    setLevel     rh p = rh { rhSeverities = p }
    setFormatter rh f = rh { rhFormatter  = f }
    getLevel          = rhSeverities
    getFormatter      = rhFormatter
    readBack          = rollerReadback

    emit rh bldr _    = liftIO $ rhWriteAction rh (error "Handler is used internally") (toText . B.toLazyText $ bldr)
    close RollerHandler{..} = liftIO $ withMVar rhFileHandle rhCloseAction

newtype InvalidRotation = InvalidRotation Text
    deriving (Show, Eq)

instance Exception InvalidRotation

logIndex :: FilePath -> Int -> FilePath
logIndex handlerPath i = handlerPath <.> show i

rollerReadback :: MonadIO m => RollerHandler -> Int -> m [Text]
rollerReadback RollerHandler{..} logsNum = liftIO $
    modifyMVar rhFileHandle $ \h -> do
        hFlush h
        hSeek h AbsoluteSeek 0
        contents <- T.lines <$> TIO.hGetContents h
        hClose h
        h' <- openFile rhFileName ReadWriteMode
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
                whenExist oldLogFile (`renameFile` newLogFile)
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
    -> Severities
    -> m RollerHandler
rotationFileHandler rp@RotationParameters{..} _ _
    | not $ isValidRotation rp = liftIO $ throwM $ InvalidRotation $
      "Rotation parameters must be positive: "+||rp||+""
rotationFileHandler rp@RotationParameters{..} handlerPath rhSeverities = liftIO $ do
    GenericHandler{..} <- fileHandler handlerPath rhSeverities
    rhFileHandle       <- newMVar privData
    let rhWriteAction   = rollerWriting rp handlerPath writeFunc rhFileHandle
    pure RollerHandler { rhFormatter   = nullFormatter
                       , rhCloseAction = closeFunc
                       , rhFileName = handlerPath
                       , ..
                       }
