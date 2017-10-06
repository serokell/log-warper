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
       ( streamHandler
       , fileHandler
       , GenericHandler(..)
       , verboseStreamHandler
       ) where

import           Control.Concurrent      (modifyMVar_, withMVar)
import           Control.Exception       (SomeException)
import qualified Data.Text.IO            as TIO
import           Data.Text.Lazy.Builder  as B
import           Data.Typeable           (Typeable)
import           System.Directory        (createDirectoryIfMissing)
import           System.FilePath         (takeDirectory)
import           System.IO               (Handle, IOMode (ReadWriteMode),
                                          SeekMode (SeekFromEnd), hClose,
                                          hFlush, hSeek)
import           Universum

import           System.Wlog.Formatter   (LogFormatter, nullFormatter,
                                          simpleLogFormatter)
import           System.Wlog.Handler     (LogHandler (..), LogHandlerTag (..))
import           System.Wlog.MemoryQueue (MemoryQueue)
import           System.Wlog.MemoryQueue as MQ
import           System.Wlog.Severity    (Severity (..))


-- | A helper data type.
data GenericHandler a = GenericHandler
    { severity       :: !Severity
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
    setFormatter sh f = sh{formatter = f}
    getFormatter sh = formatter sh
    readBack sh i = withMVar (readBackBuffer sh) $ \mq' -> pure $! take i . MQ.toList $ mq'
    emit sh bldr _ = (writeFunc sh) (privData sh) (toText . B.toLazyText $ bldr)
    close sh = (closeFunc sh) (privData sh)

-- | Create a stream log handler. Log messages sent to this handler
-- will be sent to the stream used initially. Note that the 'close'
-- method will have no effect on stream handlers; it does not actually
-- close the underlying stream.
streamHandler :: Handle -> Severity -> IO (GenericHandler Handle)
streamHandler h sev = do
    lock <- newMVar ()
    mq <- newMVar $ MQ.newMemoryQueue $ 2 * 1024 * 1024 -- 2 MB
    let mywritefunc hdl msg = withMVar lock $ const $ do
            writeToHandle hdl msg
            -- Important to force the queue here, else a massive closure will
            -- be retained until the queue is actually used.
            modifyMVar_ mq $ \mq' -> pure $! pushFront msg mq'
            hFlush hdl
    return
        GenericHandler
        { severity = sev
        , formatter = nullFormatter
        , privData = h
        , writeFunc = mywritefunc
        , closeFunc = const $ pure ()
        , readBackBuffer = mq
        , ghTag = HandlerOther "GenericHandler/StreamHandler"
        }
  where
    writeToHandle hdl msg =
        TIO.hPutStrLn hdl msg `catch` (handleWriteException hdl msg)
    handleWriteException :: Handle -> Text -> SomeException -> IO ()
    handleWriteException hdl msg e =
        let msg' =
                "Error writing log message: " <>
                show e <> " (original message: " <> msg <> ")"
        in TIO.hPutStrLn hdl msg'

-- | Create a file log handler.  Log messages sent to this handler
-- will be sent to the filename specified, which will be opened in
-- Append mode.  Calling 'close' on the handler will close the file.
fileHandler :: FilePath -> Severity -> IO (GenericHandler Handle)
fileHandler fp sev = do
    createDirectoryIfMissing True (takeDirectory fp)
    h <- openFile fp ReadWriteMode
    hSeek h SeekFromEnd 0
    sh <- streamHandler h sev
    pure $ sh { closeFunc = hClose
              , ghTag = HandlerFilelike fp
              }

-- | Like 'streamHandler', but note the priority and logger name along
-- with each message.
verboseStreamHandler :: Handle -> Severity -> IO (GenericHandler Handle)
verboseStreamHandler h sev =
    let fmt = simpleLogFormatter "[$loggername/$prio] $msg"
    in do hndlr <- streamHandler h sev
          return $ setFormatter hndlr fmt
