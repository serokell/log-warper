{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}

{- |
   Module     : System.Log.Logger
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Haskell Logging Framework, Primary Interface

Written by John Goerzen, jgoerzen\@complete.org

This module is a modification of "System.Log.Logger" of 'hslogger'
library. Unless proper description is written here, please use the
original documentation available on hackage/hslogger.
-}

module System.Wlog.Logger
       (
         -- * Basic Types
         Logger
         -- ** Re-Exported from System.Wlog
       , Severity(..)

         -- * Logging Messages
         -- ** Basic
       , logM
       , logMCond
         -- ** Utility Functions
       , debugM, infoM, noticeM, warningM, errorM
       , removeAllHandlers
       , traplogging
         -- ** Logging to a particular Logger by object
       , logL
       , logLCond

         -- * Logger Manipulation
         -- ** Finding ∨ Creating Loggers
       , getLogger, getRootLogger, rootLoggerName
         -- ** Modifying Loggers
       , addHandler, removeHandler, setHandlers
       , getLevel, setLevel, clearLevel
         -- ** Saving Your Changes
       , saveGlobalLogger
       , updateGlobalLogger
       ) where

import           Control.Concurrent.MVar    (modifyMVar, modifyMVar_)
import           Data.List                  (isPrefixOf)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromJust)
--import           System.IO
import           System.IO.Unsafe           (unsafePerformIO)
import           System.Wlog.Handler        (LogHandler (getTag), close)
import qualified System.Wlog.Handler        (handle)
import           System.Wlog.Handler.Simple (streamHandler)
import           System.Wlog.Severity       (LogRecord, Severity (..))
import           Universum


---------------------------------------------------------------------------
-- Basic logger types
---------------------------------------------------------------------------

data HandlerT = forall a. LogHandler a => HandlerT a

data Logger = Logger
    { level    :: Maybe Severity
    , handlers :: [HandlerT]
    , name     :: String
    }

type LogTree = Map.Map String Logger

---------------------------------------------------------------------------
-- Utilities
---------------------------------------------------------------------------

-- | The name of the root logger, which is always defined and present
-- on the system.
rootLoggerName :: String
rootLoggerName = ""

---------------------------------------------------------------------------
-- Logger Tree Storage
---------------------------------------------------------------------------

-- | The log tree.  Initialize it with a default root logger
-- and (FIXME) a logger for MissingH itself.
{-# NOINLINE logTree #-}
logTree :: MVar LogTree
-- note: only kick up tree if handled locally
logTree = unsafePerformIO $ do
    h <- streamHandler stderr Debug
    newMVar $
        Map.singleton rootLoggerName $
        Logger {level = Just Warning, name = "", handlers = [HandlerT h]}


{- | Given a name, return all components of it, starting from the root.
Example return value:

>["", "MissingH", "System.Cmd.Utils", "System.Cmd.Utils.pOpen"]

-}
componentsOfName :: String -> [String]
componentsOfName name =
    let joinComp [] _ = []
        joinComp (x:xs) [] = x : joinComp xs x
        joinComp (x:xs) accum =
            let newlevel = accum ++ "." ++ x
            in newlevel : joinComp xs newlevel
    in rootLoggerName : joinComp (split "." name) []

---------------------------------------------------------------------------
-- Logging With Location
---------------------------------------------------------------------------

-- | Log a message using the given logger at a given priority.
logM :: String     -- ^ Name of the logger to use
     -> Severity   -- ^ Severity of this message
     -> Text     -- ^ The log text itself
     -> IO ()
logM logname pri msg = do
    l <- getLogger logname
    logL l pri msg

logMCond :: String -> Severity -> Text -> (String -> Bool) -> IO ()
logMCond logname sev msg cond = do
    l <- getLogger logname
    logLCond l sev msg cond

---------------------------------------------------------------------------
-- Utility functions
---------------------------------------------------------------------------

{- | Log a message at 'DEBUG' priority -}
debugM :: String                         -- ^ Logger name
       -> Text                         -- ^ Log message
       -> IO ()
debugM s = logM s Debug

{- | Log a message at 'INFO' priority -}
infoM :: String                         -- ^ Logger name
      -> Text                         -- ^ Log message
      -> IO ()
infoM s = logM s Info

{- | Log a message at 'NOTICE' priority -}
noticeM :: String                         -- ^ Logger name
        -> Text                         -- ^ Log message
        -> IO ()
noticeM s = logM s Notice

{- | Log a message at 'WARNING' priority -}
warningM :: String                         -- ^ Logger name
         -> Text                         -- ^ Log message
         -> IO ()
warningM s = logM s Warning

{- | Log a message at 'ERROR' priority -}
errorM :: String                         -- ^ Logger name
       -> Text                         -- ^ Log message
       -> IO ()
errorM s = logM s Error

---------------------------------------------------------------------------
-- Public Logger Interaction Support
---------------------------------------------------------------------------

-- | Returns the logger for the given name.  If no logger with that name
-- exists, creates new loggers and any necessary parent loggers, with
-- no connected handlers.
getLogger :: String -> IO Logger
getLogger lname = modifyMVar logTree $ \lt -> case Map.lookup lname lt of
    Just x ->  return (lt, x) -- A logger exists; return it and leave tree
    Nothing -> do
        -- Add logger(s).  Then call myself to retrieve it.
        let newlt = createLoggers (componentsOfName lname) lt
        let result = fromJust $ Map.lookup lname newlt
        return (newlt, result)
  where
    createLoggers :: [String] -> LogTree -> LogTree
    createLoggers [] lt = lt -- No names to add; return tree unmodified
    createLoggers (x:xs) lt = -- Add logger to tree
        if Map.member x lt
           then createLoggers xs lt
           else createLoggers xs
                    (Map.insert x (defaultLogger {name=x}) lt)
    defaultLogger = Logger Nothing [] undefined -- ???!??!

-- | Returns the root logger.
getRootLogger :: IO Logger
getRootLogger = getLogger rootLoggerName

-- | Log a message, assuming the current logger's level permits it.
logL :: Logger -> Severity -> Text -> IO ()
logL l pri msg = handle l (pri, msg) (const True)

-- | Logs a message with condition.
logLCond :: Logger -> Severity -> Text -> (String -> Bool) -> IO ()
logLCond l pri msg = handle l (pri, msg)

-- | Handle a log request.
handle :: Logger -> LogRecord -> (String -> Bool) -> IO ()
handle l lrecord@(sev, _) handlerFilter = do
    lp <- getLoggerSeverity (name l)
    if sev >= lp then do
        ph <- parentHandlers (name l)
        forM_ ph $ callHandler lrecord (name l)
    else return ()
  where
    parentLoggers :: String -> IO [Logger]
    parentLoggers [] = return []
    parentLoggers name =
        let pname0 = (head . drop 1 . reverse . componentsOfName) name
            pname = fromMaybe (error "Logger.handle.parentLoggers: pname head failed") pname0
        in do parent <- getLogger pname
              next <- parentLoggers pname
              return (parent : next)
    parentHandlers :: String -> IO [HandlerT]
    parentHandlers name = do
        parentLoggers name >>= (return . concatMap handlers)
    -- Get the severity we should use. Find the first logger in the
    -- tree, starting here, with a set severity. If even root doesn't
    -- have one, assume "Debug".
    getLoggerSeverity :: String -> IO Severity
    getLoggerSeverity name = do
        pl <- parentLoggers name
        case catMaybes . map level $ (l : pl) of
            []    -> pure Debug
            (x:_) -> pure x
    callHandler :: LogRecord -> String -> HandlerT -> IO ()
    callHandler lr loggername (HandlerT x) =
        when (handlerFilter $ getTag x) $
        System.Wlog.Handler.handle x lr loggername


-- | Add handler to 'Logger'.  Returns a new 'Logger'.
addHandler :: LogHandler a => a -> Logger -> Logger
addHandler h l= l{handlers = (HandlerT h) : (handlers l)}

-- | Remove a handler from the 'Logger'.  Handlers are removed in the reverse
-- order they were added, so the following property holds for any 'LogHandler'
-- @h@:
--
-- > removeHandler . addHandler h = id
--
-- If no handlers are associated with the 'Logger', it is returned unchanged.
--
-- The root logger's default handler that writes every message to stderr can
-- be removed by using this function before any handlers have been added
-- to the root logger:
--
-- > updateGlobalLogger rootLoggerName removeHandler
removeHandler :: Logger -> Logger
removeHandler l = l { handlers = drop 1 $ handlers l }

-- | Set the 'Logger'\'s list of handlers to the list supplied.
-- All existing handlers are removed first.
setHandlers :: LogHandler a => [a] -> Logger -> Logger
setHandlers hl l =
    l{handlers = map (\h -> HandlerT h) hl}

-- | Returns the "level" of the logger.  Items beneath this
-- level will be ignored.
getLevel :: Logger -> Maybe Severity
getLevel = level

-- | Sets the "level" of the 'Logger'.  Returns a new
-- 'Logger' object with the new level.
setLevel :: Severity -> Logger -> Logger
setLevel p l = l {level = Just p}

-- | Clears the "level" of the 'Logger'.  It will now inherit the level of
-- | its parent.
clearLevel :: Logger -> Logger
clearLevel l = l {level = Nothing}

-- | Updates the global record for the given logger to take into
-- account any changes you may have made.
saveGlobalLogger :: Logger -> IO ()
saveGlobalLogger l = modifyMVar_ logTree
                     (\lt -> return $ Map.insert (name l) l lt)

-- | Helps you make changes on the given logger.  Takes a function
-- that makes changes and writes those changes back to the global
-- database.  Here's an example from above (\"s\" is a 'LogHandler'):
--
-- > updateGlobalLogger "MyApp.BuggyComponent"
-- >                    (setLevel DEBUG . setHandlers [s])
updateGlobalLogger
    :: String -- ^ Logger name
    -> (Logger -> Logger) -- ^ Function to call
    -> IO ()
updateGlobalLogger ln func =
    do l <- getLogger ln
       saveGlobalLogger (func l)

-- | Allow graceful shutdown. Release all opened files/handlers/etc.
removeAllHandlers :: IO ()
removeAllHandlers =
    modifyMVar_ logTree $ \lt -> do
        let allHandlers = Map.fold (\l r -> concat [r, handlers l]) [] lt
        mapM_ (\(HandlerT h) -> close h) allHandlers
        return $ Map.map (\l -> l {handlers = []}) lt

-- | Traps exceptions that may occur, logging them, then passing them on.
--
-- Takes a logger name, priority, leading description text (you can set it to
-- @\"\"@ if you don't want any), and action to run.
traplogging :: String     -- ^ Logger name
            -> Severity   -- ^ Logging priority
            -> Text       -- ^ Descriptive text to prepend to logged messages
            -> IO a       -- ^ Action to run
            -> IO a       -- ^ Return value
traplogging logger priority desc action = action `catch` handler
  where
    realdesc =
        case desc of
            "" -> ""
            x  -> x <> ": "
    handler :: SomeException -> IO a
    handler e = do
        logM logger priority (realdesc <> show e)
        throwM e -- Re-raise it

-- | This function pulled in from MissingH to avoid a dep on it
split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (isPrefixOf delim) str
    in firstline :
       case remainder of
           [] -> []
           x | x == delim -> [] : []
             | otherwise -> split delim (drop (length delim) x)

-- This function also pulled from MissingH
breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

-- This function also pulled from MissingH
spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([],[])
spanList func l@(x:xs) =
    let (ys, zs) = spanList func xs
    in if func l
       then (x : ys, zs)
       else ([], l)
