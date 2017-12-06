# `log-warper` how-to

If you want to see the working example first, please, check out this
[very simple example](https://github.com/serokell/log-warper/blob/master/examples/Playground.hs).

Here we will try to explain step by step how to integrate `log-warper` into our very small project,
specifically how to replace `putStrLn` with simple naive logging.

All code below can be compiled and run with these commands

```bash
stack build log-warper
stack exec how-to
```
and you can see how it's working both with and without logging.

## Preamble: imports and language extensions

Since this is a literate haskell file, we need to specify all our language extensions
and imports up front.

``` haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Semigroup ((<>))
import Data.Text (Text)
import System.Wlog (WithLoggerIO, runLoggingWithFile, logError, logInfo)

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

```

## Adding a new logger

First of all, you may ask what are we doing? What the hell is logger?

**Logger** is the [output handler](https://hackage.haskell.org/package/log-warper-1.7.1/docs/System-Wlog-LogHandler.html#t:LogHandler)
labeled by the unique [logger name](https://hackage.haskell.org/package/log-warper-1.7.1/docs/System-Wlog-LoggerName.html#t:LoggerName).

When you log new messages you need to specify logger which is responsible for logging this message.
Fortunately, log-warper has monadic interfaces and logger will be taken explicitly from current context
if possible, thus you need to specify logger only once for some block of actions.
Don't worry, this will be covered later!

So, if you're adding a new logger for your needs you'll have to consider several things.
Firstly, you should choose some reasonable name for the logger you are creating
according to the actions that it would cover with logging. From now on let's assume
that you need to have the logger with the name `new-logger` in your project.

### Configuration file

There are two ways of configuring the logger you creating. One option
is to configure everything directly in the code, which will make the process
of writing code less convenient. Here we will use another configuration possibility
— we will write configurations in special `.yaml` file.
In our example we will use very simple one with the minimal settings needed for the file to work.
You can have a look at [`how-to-log-config.yaml`](https://github.com/serokell/log-warper/blob/master/examples/how-to-log-config.yaml),
it contains the following lines:

```yaml
loggerTree:
    severity: Warning+    # severities for «root» logger
    new-logger:           # logger named «new-logger»
        severity: Debug+  # severities for logger «new-logger»
```
You don't need to worry about root logger for now. This part is explained in section devoted to hierarchical logging.

For full information about all configuration file's features and options please see [this section](#link-to-readme-config).

### Setting up

Before using your logging subsystem you need to set it up. To do that there are functions in `log-warper` library.
In our example we will run all functions with our logger from `main` function of `Main` module.

Let's consider that this is what we have:

```haskell
mainWithoutLogging :: IO ()
mainWithoutLogging = inputLength
```
 where `inputLength` is the function where we will add some logging messages.

So to make it possible we should transform this function to the following:

```haskell
mainWithLogging :: IO ()
mainWithLogging =
    runLoggingWithFile "examples/how-to-log-config.yaml" "new-logger" inputLengthWithLog
```
where we set up the config from the file and run `inputLengthWithLog` under the logger with name `new-logger`.
Note that we use `inputLengthWithLog` now instead of `inputLength` because we can not just add logging
to the function that works under `IO` monad automatically. We need to change our faction.
And `inputLengthWithLog` is a version of `inputLength` with logging added.

Let's have a closer look at how we transform the `inputLength` function
to the `inputLengthWithLog` that would work with logging in the next paragraph.

## Add logging messages to function in `IO`

If you want to use logging in a function that already runs in the `IO`,
replace concrete `IO` type with polymorphic constraint
[`WithLoggerIO`](https://hackage.haskell.org/package/log-warper-1.7.1/docs/System-Wlog-CanLog.html#t:WithLoggerIO)` m => m`
and add [`liftIO`](https://www.stackage.org/haddock/lts-9.14/base-4.9.1.0/Control-Monad-IO-Class.html#v:liftIO) all your `IO` actions.

For example, our `inputLength` function works this way:
```haskell

inputLength :: IO ()
inputLength = do
    input <- TextIO.getLine
    case Text.length input of
        0 -> do
            putStrLn "Should not be empty"
            inputLength
          -- v ^  you need to log these
        _ -> do
            putStrLn "Well done"
            funWithInput input

```

_**Note:**_ All logging functions take [`Text`](https://hackage.haskell.org/package/text-1.2.2.2/docs/Data-Text.html#t:Text)
messages instead of `String`. So users might convert their `String`s to `Text`s with
[`Text.pack`](https://hackage.haskell.org/package/text-1.2.2.2/docs/Data-Text.html#v:pack)
or migrate to the `text` package or use some custom prelude which makes the work with `Text` more
convenient, e.g. [`universum`](https://hackage.haskell.org/package/universum).

Okay, it's very easy function. We have some input from console and write some messages depending of the length of user's input.
What we are going to do is to log these messages instead of just `putStrLn` them.

First that we should consider is that logging can not be done in `IO`, so we need make
the function work in some polymorphic monad, which implements `WithLoggerIO` constraint,
so that we are able to do any logging stuff inside.
After this is done you can easily use `logDebug`, `logInfo`, `logWarning` etc.
(see others [here](https://hackage.haskell.org/package/log-warper-1.7.1/docs/System-Wlog-CanLog.html#v:logDebug)) to log messages.
As it's not `IO` after our transformations, we shouldn't forget to `liftIO` functions that work in `IO` and doesn't have logging inside.
As result we will get

```haskell

inputLengthWithLog :: WithLoggerIO m => m ()
inputLengthWithLog = do
    input <- liftIO TextIO.getLine
    case Text.length input of
        0 -> do
            -- `putStrLn` is replaced with `logError` now.
            logError "Should not be empty"
            inputLengthWithLog
        _ -> do
            -- `putStrLn` is replaced with `logInfo` now.
            logInfo "Well done"
            funWithInputInMonad input

```

Another moment, let's have a closer look at the `funWithInput` function from the example above.
We see that its type is `IO ()` and it doesn't have logging inside.

```haskell
funWithInput :: Text -> IO ()
funWithInput input = do
    TextIO.putStrLn $ "Wow! You wrote " <> input

```
So we also can rewrite it in this way
```haskell
funWithInputInMonad :: MonadIO m => Text -> m ()
funWithInputInMonad input =
    liftIO $ TextIO.putStrLn $ "Wow! You wrote " <> input

```
or usage of `liftIO` is also the solution.

And as a conclusion, let's write the `main` function to check how we managed to apply logging in very simple example
```haskell
main :: IO ()
main = do
    mainWithoutLogging
    mainWithLogging
```

You can see an example of the work of the programm we've just wrote:
![Example of run](https://user-images.githubusercontent.com/8126674/33295654-bd6cc94e-d3e7-11e7-8c03-e54aa6556f78.png)
