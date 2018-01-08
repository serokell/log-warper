log-warper
==========

[![Build Status](https://travis-ci.org/serokell/log-warper.svg)](https://travis-ci.org/serokell/log-warper)
[![Hackage](https://img.shields.io/hackage/v/log-warper.svg)](https://hackage.haskell.org/package/log-warper)
[![Stackage LTS](http://stackage.org/package/log-warper/badge/lts)](http://stackage.org/lts/package/log-warper)
[![Stackage Nightly](http://stackage.org/package/log-warper/badge/nightly)](http://stackage.org/nightly/package/log-warper)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

`log-warper` is a high level and featureful logging library with monadic interface.

You can jump right into [introduction tutorial](https://github.com/serokell/log-warper/blob/master/examples/HowTo.md)
to see how to write logging with `log-warper`.

## Features

Here is the list of feature `log-warper` provides.

1. Hierarchical logger names.

   Logger names (tags for loggers) forms hierarchy. It means, that `""`
   (also known as `mempty` or `rootLoggerName`) is a parent of logger with name `"node"` which isDir
   a parent of logger with name `"node.communication"`. So, logger name comprises dot-separated components.
   This means that if some logger name doesn't have some settings (like severity or output file) it takes
   its settings from its closest parent, containing this settings.

2. Logging initialization from `.yaml` configuration file.

   Whole logging configuration can be specifed in a single `.yaml` file.
   See some [simple example here](https://github.com/input-output-hk/cardano-sl/blob/develop/log-config-prod.yaml)

3. Monadic logging interface.

   `log-warper` uses `mtl`-style type classes to provide monadic interfaces for logging.

4. Strict `StateT` based pure logging.

   See [this tutorial](https://github.com/serokell/log-warper/blob/master/examples/PureLogging.md)
   on pure logging with `log-warper`.

5. Different severity levels of messages with the ability to configure `Set` of severities.

6. Output is colored :star:

   When you log messages, you see time of this logging message, logger name, severity and `ThreadId`.
   Message formatting is configurable. Color or logged message tag depends on `Severity` for this message.

7. Flexible and easy creation of `LoggerConfig` using monoidal builders and lenses.

   In case `.yaml` configuration is not enough for you, you can use `lens`-based EDSL to create configurations.
   `LoggerConfig` also implements instances for `Semigroup` and `Monoid` so you can combine your configurations
   from different sources (CLI and `.yaml` for example).

8. Logger rotation.

   `log-warper` supports logger rotation. Yes, there exist `logrotate` and similar tools.
   But it's not easy to configure cross-platform (Windows, Linux, OSX) logging rotation with external tools.

9. Ability to acquire last `N` megabytes of logs from in-memory cache.

   In case you want to analyze logging messages you can take them from in-memory cache.

## Contributing

> **This project uses [`universum`](https://github.com/serokell/universum)
> as default prelude**
