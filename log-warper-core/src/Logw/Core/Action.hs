{-# LANGUAGE InstanceSigs #-}

-- | Implements general type for logging action.

module Logw.Core.Action
       ( -- * Core type and instances
         LogAction (..)

         -- * Combinators
       , joinActions
       , ward
       , cmap
       , cbind
       ) where

import Control.Monad (when, (>=>))
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..), stimesMonoid)

----------------------------------------------------------------------------
-- Core data type with instances
----------------------------------------------------------------------------

{- | Polymorphic and very general logging action type.

@__msg__@ type variables is an input for logger. It can be 'Text' or custom
logging messsage with different data types you want to format in future.

@__m__@ type variable is for monadic action inside which logging is happening. It
can be either 'IO' or some custom pure monad.

Key point of design here is that 'LogAction' is an instance of 'Semigroup' type
class. It's supposed to be used like this:

@
__main__ :: 'IO' ()
__main__ = __do__
  action \<- logFromConfig "config.yaml" <> logElasticSearch <> logToStdout
  launchLogging action application
@
-}
newtype LogAction m msg = LogAction
    { unLogAction :: msg -> m ()
    }

instance Applicative m => Semigroup (LogAction m a) where
    (<>) :: LogAction m a -> LogAction m a -> LogAction m a
    LogAction action1 <> LogAction action2 = LogAction $ \a -> action1 a *> action2 a
    {-# INLINE (<>) #-}

    sconcat :: NonEmpty (LogAction m a) -> LogAction m a
    sconcat = joinActions
    {-# INLINE sconcat #-}

    stimes :: Integral b => b -> LogAction m a -> LogAction m a
    stimes = stimesMonoid
    {-# INLINE stimes #-}

instance Applicative m => Monoid (LogAction m a) where
    mappend :: LogAction m a -> LogAction m a -> LogAction m a
    mappend = (<>)
    {-# INLINE mappend #-}

    mempty :: LogAction m a
    mempty = LogAction $ \_ -> pure ()
    {-# INLINE mempty #-}

    mconcat :: [LogAction m a] -> LogAction m a
    mconcat = joinActions
    {-# INLINE mconcat #-}

----------------------------------------------------------------------------
-- Combinators
----------------------------------------------------------------------------

-- TODO: should be 'Container' here for better compatibility with 'universum'?..
-- | Joins some 'Foldable' of 'LogAction's into single 'LogAction'.
joinActions :: (Foldable t, Applicative m) => t (LogAction m a) -> LogAction m a
joinActions actions = LogAction $ \a -> for_ actions $ \(LogAction action) -> action a
{-# INLINE joinActions #-}
{-# SPECIALIZE joinActions :: Applicative m => [LogAction m a]          -> LogAction m a #-}
{-# SPECIALIZE joinActions :: Applicative m => NonEmpty (LogAction m a) -> LogAction m a #-}

-- | Takes predicate and performs given logging action only if predicate returns
-- 'True' on input logging message.
ward :: Applicative m => (msg -> Bool) -> LogAction m msg -> LogAction m msg
ward predicate (LogAction action) = LogAction $ \a -> when (predicate a) (action a)
{-# INLINE ward #-}

{- | This combinator is @contramap@ from contravariant functor. 'LogAction' is
an instance of @Contravariant@ but we don't want to depend on extra packages if
it's not really neccessary.

This combinator is useful when you have something like

@
__data__ LogRecord = LR
    { lrName    :: LoggerName
    , lrMessage :: Text
    }
@

and you need to provide 'LogAction' which consumes @LogRecord@

@
'LogAction' m LogRecord
@

when you only have action which consumes 'Text'

@
myAction :: 'LogAction' m Text
@

So you can just

@
'cmap' lrMesssage myAction :: 'LogAction' m LogRecord
@
-}
cmap :: (r -> e) -> LogAction m e -> LogAction m r
cmap f (LogAction action) = LogAction (action . f)
{-# INLINE cmap #-}

{- | This combinator is like 'cmap' but for function which requires extra
context to extend consumed value. Consider the following example.

You have this logging record:

@
__data__ LogRecord = LR
    { lrTime    :: UTCTime
    , lrMessage :: Text
    }
@

and you also have logging consumer inside 'IO' for such record:

@
myAction :: 'LogAction' 'IO' LogRecord
@

But you need to return consumer for only 'Text' messages:

@
'LogAction' 'IO' Text
@

But if you have function like this:

@
__withTime__ :: 'Text' -> 'IO' LogRecord
__withTime__ msg = __do__
    time <- getCurrentTime
    pure (LR time msg)
@

you can achieve desired behavior in the following way:

@
'cbind' withTime myAction :: 'LogAction' 'IO' Text
@
-}
cbind :: Monad m => (a -> m b) -> LogAction m b -> LogAction m a
cbind f (LogAction action) = LogAction (f >=> action)
{-# INLINE cbind #-}
