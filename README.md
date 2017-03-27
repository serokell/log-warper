log-warper
==========

[![Hackage](https://img.shields.io/hackage/v/log-warper.svg)](https://hackage.haskell.org/package/log-warper)
[![Build Status](https://travis-ci.org/serokell/log-warper.svg)](https://travis-ci.org/serokell/log-warper)

Auxilary logging library, which is wrapper over
[hslogger](http://hackage.haskell.org/package/hslogger) but allows
to keep logger name into monadic context, making logging less boilerplate.

Key features:

1. Output is colored :star:
2. Supports logging initialization from `.yaml` configuration file
3. Has wrapper for pure logging via `WriterT`
4. Supports log rotation
5. Flexibles and easy setting up of loggers (using monoidal builders and lenses)
6. Ability to acquire last `N` megabytes of logs from in-memory cache
