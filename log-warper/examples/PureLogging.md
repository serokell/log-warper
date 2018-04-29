# Pure logging

One of the features that `log-warper` supports is *pure logging*.
All related documentation and functions can be found in the [`PureLogging`][pure] module.
In this document we will try to show how to use logging features
in the pure functions.

All the code below can be compiled and run with these commands

```bash
stack build log-warper
stack exec pure-how-to
```
and you would see the logging output.

## Preamble: imports and language extensions

Since this is a literate haskell file, we need to specify all our language extensions
and imports up front.

``` haskell
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where


import Control.Monad.State.Strict (MonadState (..), State, evalState, get, lift, modify')
import Data.Semigroup ((<>))

import System.Wlog (NamedPureLogger, WithLogger, WithLoggerIO, launchNamedPureLogWith,
                    launchSimpleLogging, logDebug, logInfo)

import qualified Data.Text as Text

```

## Pure code

Let's consider that you have some pure monadic action where you want to add logging to.
You can't use `putStrLn` or anything like that because you don't have `IO` in your type.
But you still need to be able to inspect intermediate states of your computation.
That's why you need another feature to have some ability to log some information there.

Let's have a closer look at that.

So, we will have some simple pure `Monad` for our simple example.
Imagine that we have a newtype `PureCounter` which is actually wrapper on `State Int`
where `Int` represents some accumulator.

```haskell

-- | Pure monad to work with counter.
newtype PureCounter a = PureCounter
    { runPureCounter :: State Int a
    } deriving (Functor, Applicative, Monad)
```

Also we have a class `MonadCounter` where it will all work in.
With this class we will be able to get the current counter
and to increase the counter by one

```haskell

-- | 'MonadCounter' class.
class Monad m => MonadCounter m where
    -- | Return the current counter.
    getCounter :: m Int
    -- | Returns the resulting counter after incrementation.
    incCounter :: m Int

```

Let's implement the instance for our data type

```haskell

instance MonadCounter PureCounter where
    getCounter = PureCounter $ get
    incCounter = PureCounter $ modify' (+ 1) >> get

```

We need a way to evaluate our pure computation, so let's add such function.

```haskell

-- | Evaluates the 'PureCounter'.
evalPureCounter :: PureCounter a -> a
evalPureCounter pc = evalState (runPureCounter pc) 0

```
And of course let's have a function which works only for `MonadCounter` for testing purposes

```haskell

-- | Increases the counter twice.
doubleInc :: MonadCounter m => m Int
doubleInc = incCounter >> incCounter

```

That `doubleInc` seems to be good candidate to add some logging, cause
it's doing some smart stuff and it would be great to have that info logged properly.

```haskell

doubleIncWithLog :: (MonadCounter m, WithLogger m) => m ()
doubleIncWithLog = do
    counterBefore <- getCounter
    logDebug $ "Before incrementation " <> Text.pack (show counterBefore)
    counterAfter <- doubleInc
    logInfo $ "After incrementation " <> Text.pack (show counterAfter)

```
You see how we added [`logDebug`][logDebug] and [`logInfo`][logInfo]
functions into our code. But these functions demands `m` to be in
class `WithLogger`.

In order to actually observe the results of logging we need
to print the result inside the `IO` monad thus we need
to have constraint `WithLoggerIO`.
Let's add such function that will be called from `main`.

```haskell

-- | Runs 'doubleIncWithLog' with logging handling.
runCounterWithLog :: (WithLoggerIO m) => m ()
runCounterWithLog =
    launchNamedPureLogWith evalPureCounter doubleIncWithLog

```

Here you see [`launchNamedPureLogWith`][lnplw] which is basically
makes logging happen. It gathers all the logs that were written in
`doubleIncWithLog` and will take them out of there.

Also take into consideration that along  with `launchNamedPureLogWith`
there is [`launchNamedPureLog`][lnpl] for your needs.

We also need to implement instance of `MonadCounter` for [`NamedPureLogger`][NPL]
data type from `log-warper` library to be able to launch pure logging.
This data type is responsible for collecting logs. Hopefully, implementing
instances for this data type is rather easy!

```haskell

instance MonadCounter m => MonadCounter (NamedPureLogger m) where
    getCounter = lift getCounter
    incCounter = lift incCounter

```

And will launch our function with default logging configurations (see [`launchSimpleLogging`][simple] function).

```haskell

main :: IO ()
main = launchSimpleLogging "counter" runCounterWithLog

```

So in this example we wanted to share the ideas of usage of logging subsystem
in pure environment. See ho it's working:

```bash
$ stack exec pure-how-to
[counter:DEBUG] [2017-12-21 13:13:18.30 UTC] Before incrementation 0
[counter:INFO] [2017-12-21 13:13:18.30 UTC] After incrementation 2

```



[pure]: http://hackage.haskell.org/package/log-warper-1.8.2/docs/System-Wlog-PureLogging.html
[logDebug]: https://hackage.haskell.org/package/log-warper-1.8.2/docs/System-Wlog-CanLog.html#v:logDebug
[logInfo]: https://hackage.haskell.org/package/log-warper-1.8.2/docs/System-Wlog-CanLog.html#v:logInfo
[lnplw]: https://hackage.haskell.org/package/log-warper-1.8.2/docs/System-Wlog-PureLogging.html#v:launchNamedPureLogWith
[lnpl]: https://hackage.haskell.org/package/log-warper-1.8.2/docs/System-Wlog-PureLogging.html#v:runNamedPureLog
[NPL]: https://hackage.haskell.org/package/log-warper-1.8.2/docs/System-Wlog-PureLogging.html#t:NamedPureLogger
[simple]: https://hackage.haskell.org/package/log-warper-1.8.2/docs/System-Wlog-Launcher.html#v:launchSimpleLogging
