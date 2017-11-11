{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

{- |
   Module     : System.Log.Handler.Simple
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Simple log handlers

Written by John Goerzen, jgoerzen\@complete.org
-}

module System.Wlog.Handler.Simple
       ( GenericHandler(..)
       , defaultHandleAction

         -- * Custom handlers
       , fileHandler
       , streamHandler
       ) where

import           Control.Concurrent      (modifyMVar_, withMVar)
import           Control.Exception       (SomeException)
import qualified Data.Text.IO            as TIO
import           Data.Text.Lazy.Builder  as B
import           Data.Typeable           (Typeable)
import           System.Directory        (createDirectoryIfMissing)
import           System.FilePath         (takeDirectory)
import           System.IO               (Handle, IOMode (ReadWriteMode),
                                          SeekMode (SeekFromEnd), hClose, hFlush, hSeek)
import           Universum

import           System.Wlog.Formatter   (LogFormatter, nullFormatter)
import           System.Wlog.Handler     (LogHandler (..), LogHandlerTag (..))
import           System.Wlog.MemoryQueue (MemoryQueue)
import           System.Wlog.MemoryQueue as MQ
import           System.Wlog.Severity    (Severity (..))

-- | A helper data type.
data GenericHandler a = GenericHandler
    { severity       :: !Severity
    , printErr       :: !Bool
    , formatter      :: !(LogFormatter (GenericHandler a))
    , privData       :: !a
    , writeFunc      :: !(a -> Text -> IO ())
    , closeFunc      :: !(a -> IO ())
    , readBackBuffer :: !(MVar (MemoryQueue Text))
    , ghTag          :: !LogHandlerTag
    } deriving Typeable

instance Typeable a => LogHandler (GenericHandler a) where
    getTag = ghTag
    setLevel sh s = sh {severity = s}
    getLevel sh = severity sh
    shouldPrintError a = printErr a
    setFormatter sh f = sh{formatter = f}
    getFormatter sh = formatter sh
    readBack sh i = withMVar (readBackBuffer sh) $ \mq' -> pure $! take i . MQ.toList $ mq'
    emit sh bldr _ = (writeFunc sh) (privData sh) (toText . B.toLazyText $ bldr)
    close sh = (closeFunc sh) (privData sh)

-- | Default action which just prints to handle using given message.
defaultHandleAction :: Handle -> Text -> IO ()
defaultHandleAction h message =
    TIO.hPutStrLn h message `catch` handleWriteException
  where
    handleWriteException :: SomeException -> IO ()
    handleWriteException e = do
        let errorMessage = "Error writing log message: "
                        <> show e <> " (original message: " <> message <> ")"
        TIO.hPutStrLn h errorMessage

-- | Creates custom write action and memory queue where write action
-- updates memory queue as well.
createWriteFuncWrapper
    :: (Handle -> Text -> IO ())
    -> MVar ()
    -> IO ( Handle -> Text -> IO ()
          , MVar (MemoryQueue Text)
          )
createWriteFuncWrapper action lock = do
    memoryQueue <- newMVar $ MQ.newMemoryQueue $ 2 * 1024 * 1024 -- 2 MB

    let customWriteFunc :: Handle -> Text -> IO ()
        customWriteFunc hdl msg = withMVar lock $ const $ do
            action hdl msg

            -- Important to force the queue here, else a massive closure will
            -- be retained until the queue is actually used.
            modifyMVar_ memoryQueue $ \mq -> pure $! pushFront msg mq

            hFlush hdl

    return (customWriteFunc, memoryQueue)

-- | Create a stream log handler. Log messages sent to this handler
-- will be sent to the stream used initially. Note that the 'close'
-- method will have no effect on stream handlers; it does not actually
-- close the underlying stream.
streamHandler :: Handle
              -> (Handle -> Text -> IO ())
              -> (Handle -> Bool)
              -> MVar ()
              -> Severity
              -> IO (GenericHandler Handle)
streamHandler privData writeAction shouldPrintErr lock severity = do
    (writeFunc, readBackBuffer) <- createWriteFuncWrapper writeAction lock
    return GenericHandler
        { formatter = nullFormatter
        , closeFunc = const $ pure ()
        , ghTag     = HandlerOther "GenericHandler/StreamHandler"
        , printErr  = shouldPrintErr privData
        , ..
        }

-- | Create a file log handler.  Log messages sent to this handler
-- will be sent to the filename specified, which will be opened in
-- Append mode.  Calling 'close' on the handler will close the file.
fileHandler :: FilePath -> Severity -> IO (GenericHandler Handle)
fileHandler fp sev = do
    createDirectoryIfMissing True (takeDirectory fp)
    h <- openFile fp ReadWriteMode
    hSeek h SeekFromEnd 0

    lock <- newMVar ()
    sh <- streamHandler h defaultHandleAction (const True) lock sev
    pure $ sh { closeFunc = hClose
              , ghTag = HandlerFilelike fp
              }
