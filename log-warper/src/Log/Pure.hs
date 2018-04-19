{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types      #-}

-- | This module contains implementation of @log-warper-core@ for pure monad.

module Log.Pure
       ( -- * Pure logging manipulation
         PureLoggerT (..)
       , runPureLog
--       , dispatchEvents
--       , logEvents
--       , launchPureLog

--       , NamedPureLogger (..)
--       , runNamedPureLog
--       , launchNamedPureLog
--       , launchNamedPureLogWith
--       , usingNamedPureLogger
--       , logPureAction
       ) where

import Universum

import Control.Monad.State.Strict (modify')
import Data.Sequence ((|>))

import Log.Core.Action (LogAction (..))
import Log.Core.Class (ActionT, usingActionT)
import Log.Event (Event (..))

-- | Underlying monad for pure logging action Instead of writing log message to
-- console this monad appends them to 'Seq' inside 'StateT' context.
newtype PureLoggerT exts m a = PureLoggerT
    { runPureLoggerT :: StateT (Seq (Event exts)) m a
    } deriving (Functor, Applicative, Monad, MonadTrans)

-- 'LogAction' which uses 'PureLoggerT' underhood
pureLogAction :: Monad m => LogAction (PureLoggerT exts m) (Event exts)
pureLogAction = LogAction $ \event -> PureLoggerT $ modify' (|> event)

-- | Return log of pure logging action as list of 'LogEvent'.
runPureLog :: Monad m => ActionT (Event exts) (PureLoggerT exts m) a -> m (a, [Event exts])
runPureLog = fmap (second toList)
           . usingStateT mempty
           . runPureLoggerT
           . usingActionT pureLogAction

-- -- | Logs all 'LogEvent'`s from given list. This function supposed to
-- -- be used after 'runPureLog'.
-- dispatchEvents :: MonadLogger m LogEvent => [LogEvent] -> m ()
-- dispatchEvents = mapM_ dispatchLogEvent
--   where
--     dispatchLogEvent (LogEvent name sev t) = dispatchMessage name sev t
--
-- -- | Logs all 'LogEvent'`s from given list. Just like
-- -- 'dispatchEvents' but uses proper logger Name.
-- logEvents :: WithLogger m => [LogEvent] -> m ()
-- logEvents events = do
--     logName <- askLoggerName
--     mapM_ (dispatchLogEvent logName) events
--   where
--     dispatchLogEvent logName (LogEvent _ sev t) = dispatchMessage logName sev t
--
-- -- | Performs actual logging once given action completes.
-- launchPureLog
--     :: (CanLog n, Monad m)
--     => (forall f. Functor f => m (f a) -> n (f b))
--     -> PureLogger m a
--     -> n b
-- launchPureLog hoist' action = do
--     (logs, res) <- hoist' $ swap <$> runPureLog action
--     res <$ dispatchEvents logs
--
-- newtype NamedPureLogger m a = NamedPureLogger
--     { runNamedPureLogger :: PureLogger (LoggerNameBox m) a
--     } deriving (Functor, Applicative, Monad, MonadState (Seq LogEvent),
--                 MonadThrow, HasLoggerName)
--
-- instance MonadTrans NamedPureLogger where
--     lift = NamedPureLogger . lift . lift
--
-- instance Monad m => CanLog (NamedPureLogger m) where
--     dispatchMessage name sev msg =
--         NamedPureLogger $ dispatchMessage name sev msg
--
-- instance MFunctor NamedPureLogger where
--     hoist f = NamedPureLogger . hoist (hoist f) . runNamedPureLogger
--
-- -- | Return log of pure logging action as list of 'LogEvent',
-- -- using logger name provided by context.
-- runNamedPureLog
--     :: (Monad m, HasLoggerName m)
--     => NamedPureLogger m a -> m (a, [LogEvent])
-- runNamedPureLog (NamedPureLogger action) =
--     askLoggerName >>= (`usingLoggerName` runPureLog action)
--
-- {- | Similar to 'launchPureLog', but provides logger name from current context.
--
-- Running the 'NamedPureLogger' gives us the pair  of target and the list of 'LogEvent's,
-- wrapped in 'Monad' from where using the fact that @(,)@ is 'Functor' logging can be triggered.
--
-- ==== __Example__
--
-- @
--   newtype PureSmth a = ...
--       deriving (MonadSmth, ...)
--
--   instance MonadSmth m => MonadSmt (NamedLoggerName m)
--
--   evalPureSmth :: PureSmth a -> a
--
--   makeField    :: MonadSmth m => Data -> m Field
--
--   run :: (MonadIO m, WithLogger m) => m ()
--   run = do
--       data  <- getData
--       -- field :: Field
--       field <- launchNamedPureLog (pure . evalPureSmth) (makeField data)
--       --       ^ logging happens here
--       ...
-- @
--
-- -}
-- launchNamedPureLog
--     :: (WithLogger n, Monad m)
--     => (forall f. Functor f => m (f a) -> n (f b))
--     -> NamedPureLogger m a
--     -> n b
-- launchNamedPureLog hoist' namedPureLogger = do
--     name <- askLoggerName
--     (logs, res) <- hoist' $ swap <$> usingNamedPureLogger name namedPureLogger
--     res <$ dispatchEvents logs
--
-- {- | Similar to 'launchNamedPureLog', but calls 'pure' on passed function result.
--
-- ==== __Example__
--
-- The example from 'launchNamedPureLog' with usage of this function will look like:
--
-- @
--   newtype PureSmth a = ...
--       deriving (MonadSmth, ...)
--
--   instance MonadSmth m => MonadSmt (NamedLoggerName m)
--
--   evalPureSmth :: PureSmth a -> a
--
--   makeField    :: MonadSmth m => Data -> m Field
--
--   run :: (MonadIO m, WithLogger m) => m ()
--   run = do
--       data  <- getData
--       -- field :: Field
--       field <- launchNamedPureLogWith evalPureSmth $ makeField data
--       --       ^ logging happens here
--       ...
-- @
--
-- -}
-- launchNamedPureLogWith
--     :: (WithLogger n, Monad m)
--     => (forall f. Functor f => m (f a) -> f b)
--     -> NamedPureLogger m a
--     -> n b
-- launchNamedPureLogWith hoist' = launchNamedPureLog (pure . hoist')
--
-- -- | Similar to 'runNamedPureLog', but using provided logger name.
-- usingNamedPureLogger :: Functor m
--                      => LoggerName
--                      -> NamedPureLogger m a
--                      -> m (a, [LogEvent])
-- usingNamedPureLogger name (NamedPureLogger action) =
--     usingLoggerName name $ runPureLog action
--
-- -- | Perform pure-logging computation, log its events
-- -- and return the result of the computation
-- logPureAction :: WithLogger m => NamedPureLogger m a -> m a
-- logPureAction namedPureLogger = do
--     loggerName  <- askLoggerName
--     (a, events) <- usingNamedPureLogger loggerName namedPureLogger
--     a <$ dispatchEvents events
