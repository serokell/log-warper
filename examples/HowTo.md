# `log-warper` how-to

If you want to see the working example first, please, check out this [very simple example](https://github.com/serokell/log-warper/blob/master/examples/Playground.hs).

Here we will try to explain step by step how to integrate `log-warper` into your project.

All code below can be compiled and run with these commands

```bash
stack build log-warper
stack exec how-to
```
and you can see how it's working both with and without logging.

Since this is a literate haskell file, we need to specify all our language extensions and imports up front.

``` haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid ((<>))
import Data.Text (Text)
import System.Wlog (WithLoggerIO, buildAndSetupYamlLogging, logError,
                    logInfo,productionB, usingLoggerName)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

```

## Adding a new logger

First of all, you may ask what are we doing? What the hell is logger?

**Logger** is the [output handler](https://hackage.haskell.org/package/log-warper-1.7.1/docs/System-Wlog-LogHandler.html#t:LogHandler) labeled by the unique [logger name](https://hackage.haskell.org/package/log-warper-1.7.1/docs/System-Wlog-LoggerName.html#t:LoggerName).

To add a new logger for your needs you'll have to consider several things.
Firstly, you should consider choosing some reasonable name for the logger you creating according to the actions that it would cover with logging. From now on let's assume that you need to have `new-logger` in your project.

### Configuration file

To configure your logger you will need configuration `.yaml` file.
In our example we will use very simple one with the minimal settings needed for the file to work:

```yaml
loggerTree:
    severity: Warning+    # severities for «root» logger
    new-logger:           # logger named «new-logger»
        severity: Debug+  # severities for logger «new-logger»
```

For full information about all configuration file's features and options please see [this section](#link-to-readme-config).

### Setting up

Before using your logger you need to set it up. To do that there are very useful functions in `log-warper`.
In our example we will run all functions with our logger from `main` function of `Main` module.

Let's consider that this is what we have:

```haskell
mainWithoutLogging :: IO ()
mainWithoutLogging = inputLength
```
 where `inputLength` is the function where we will add some logging messages.

So to make it possible we should transformed this function to the following:

```haskell
mainWithLogging :: IO ()
mainWithLogging = do
    buildAndSetupYamlLogging productionB "examples/how-to-log-config.yaml"
    usingLoggerName "new-logger" inputLengthWithLog
```
where we set up the config from the file and run `inputLengthWithLog` under the logger with name `new-logger`.

Let's have a closer look at how we transform the `inputLength` function to the `inputLengthWithLog` that would work with logging in the next paragraph.

## Add logging messages to function in `IO`

If you want to use logging in a function that already runs in the `IO` monad, replace the monad with [`WithLoggerIO`](https://hackage.haskell.org/package/log-warper-1.7.1/docs/System-Wlog-CanLog.html#t:WithLoggerIO) and `liftIO` all your `IO` actions.

For example, our `inputLength` function works this way:
```haskell

inputLength :: IO ()
inputLength = do
    input <- TIO.getLine
    case T.length input of
        0 -> putStrLn "Should not be empty" >> inputLength
          -- v ^  you need to log these
        _ -> putStrLn "Well done" >> funWithInput input

```
Very easy, nothing interesting. We have some input from console and write some messages depending of the length of user's input.
What we are going to do is to log this messages instead of just `putStrLn` them.

First that we should consider is that logging can not be done in `IO`, so we need make the function work in `WithLoggerIO` monad,
so that we are able to do any logging stuff inside.
After this is done you can easily use `logDebug`, `logInfo`, `logWarning` etc.
(see others [here](https://hackage.haskell.org/package/log-warper-1.7.1/docs/System-Wlog-CanLog.html#v:logDebug)) to log messages.
As it's not `IO` after our transformations, we shouldn't forget to `liftIO` functions that work in `IO` and doesn't have logging inside.
As result we will get

```haskell

inputLengthWithLog :: WithLoggerIO m => m ()
inputLengthWithLog = do
    input <- liftIO TIO.getLine
    case T.length input of
        0 -> do
            logError "Should not be empty"
            inputLengthWithLog
        _ -> do
            logInfo "Well done"
            funWithInputInMonad input

```

Another moment, let's have a closer look at the `funWithInput` function from the example above.
We see that its type is `IO ()` and it doesn't have logging inside.

```haskell
funWithInput :: Text -> IO ()
funWithInput input = do
    TIO.putStrLn $ "Wow! You wrote " <> input

```
So we also can rewrite it in this way
```haskell
funWithInputInMonad :: MonadIO m => Text -> m ()
funWithInputInMonad input =
    liftIO $ TIO.putStrLn $ "Wow! You wrote " <> input

```
or usage of `liftIO` is also the solution.

And as a conclusion, let's write the `main` function to check how we managed to apply logging in very simple example
```haskell
main :: IO ()
main = do
    mainWithoutLogging
    mainWithLogging
```
