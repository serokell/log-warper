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

import           Control.Concurrent    (withMVar)
import           Control.Exception     (SomeException)
import           Data.Maybe            (fromMaybe)
import qualified Data.Text.IO          as TIO
import           Data.Typeable         (Typeable)
import           System.IO             (hClose, hFlush)
import           Universum

import           System.Wlog.Formatter (LogFormatter, nullFormatter, simpleLogFormatter)
import           System.Wlog.Handler   (LogHandler (..))
import           System.Wlog.Severity  (Severity (..))


-- | A helper data type.
data GenericHandler a = GenericHandler
    { severity  :: Severity
    , formatter :: LogFormatter (GenericHandler a)
    , privData  :: a
    , writeFunc :: a -> Text -> IO ()
    , closeFunc :: a -> IO ()
    , ghTag     :: Maybe String
    } deriving Typeable

instance Typeable a => LogHandler (GenericHandler a) where
    getTag = fromMaybe "GenericHandler" . ghTag
    setLevel sh s = sh {severity = s}
    getLevel sh = severity sh
    setFormatter sh f = sh{formatter = f}
    getFormatter sh = formatter sh
    emit sh (_,msg) _ = (writeFunc sh) (privData sh) msg
    close sh = (closeFunc sh) (privData sh)

-- | Create a stream log handler. Log messages sent to this handler
-- will be sent to the stream used initially. Note that the 'close'
-- method will have no effect on stream handlers; it does not actually
-- close the underlying stream.
streamHandler :: Handle -> Severity -> IO (GenericHandler Handle)
streamHandler h sev = do
    lock <- newMVar ()
    let mywritefunc hdl msg =
            withMVar lock (const $ writeToHandle hdl msg >> hFlush hdl)
    return
        GenericHandler
        { severity = sev
        , formatter = nullFormatter
        , privData = h
        , writeFunc = mywritefunc
        , closeFunc = const $ pure ()
        , ghTag = Just "streamHandler"
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
    h <- openFile fp AppendMode
    sh <- streamHandler h sev
    return (sh {closeFunc = hClose, ghTag = Just ("fileHandler:" ++ fp)})

-- | Like 'streamHandler', but note the priority and logger name along
-- with each message.
verboseStreamHandler :: Handle -> Severity -> IO (GenericHandler Handle)
verboseStreamHandler h sev =
    let fmt = simpleLogFormatter "[$loggername/$prio] $msg"
    in do hndlr <- streamHandler h sev
          return $ setFormatter hndlr fmt
