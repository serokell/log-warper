1.7.2
=====

* [#57](https://github.com/serokell/log-warper/issues/57):
  Add `Exception` module with `logException` and `catchLog` functions.
* [#60](https://github.com/serokell/log-warper/issues/60):
  Fix documentation for `termSeveritiesOut` and `termSeveritiesErr`.

1.7.1
=====

* Bump `containers` to version `0.5.10`.

1.7.0
=====

* [#48](https://github.com/serokell/log-warper/issues/48):
  Output for severities is now configured in config file with `termSeveritiesOut`
  and `termSeveritiesErr` for writing into `stdout` and `stderr` accordingly.
  Default behavior: `Errors` into `stderr`, all other into `stdout`.
* In yaml config file added new keywords for dealing with `Severities`:
  'All' -- all severities, 'X+' -- severities greater or equal to X.
* Changed .yaml format: logger severity receives set of severities (`Severities`).
* [#32](https://github.com/serokell/log-warper/issues/32):
  Changed .yaml format: `LoggerTree` should be written under 'loggerTree:'.
* [#49](https://github.com/serokell/log-warper/issues/49):
  Add `WithLoggerIO` constraint.
* [#50](https://github.com/serokell/log-warper/issues/50):
  Add `liftLogIO` function into `CanLog` module.

1.6.0
=====

* `Error` is now printed only to `stderr`, all other messages to `stdout`.
* `Logger` severity is now `Set Severity`.
* Interface changes: functions which worked with `Severity` now work with `Set Severity`.
* Remove `releaseAllHandlers`, `streamHandlerWithLock`,
  `trapLogging`, `debugM`, `errorM`, `infoM`, `noticeM`, `warningM`.
* Rename `Wrapper` module to `Terminal`.
* Rename `Handler` module to `LogHandler`.
* Rename `Logger` module to `IOLogger`.
* Move `setSeverity` and `setSeverityMaybe` to `IOLogger`.
* Lift all functions inside `IOLogger` module to `MonadIO`.
* `handle` from `LogHandler` module is renamed to `logHandlerMessage`
  and moved out of type class `LogHandler`.

1.5.3
=====

* Add `launchNamedPureLogWith` to `PureLogging`
* Improve documentation for `launchNamedPureLog`

1.5.2
=====

* Add `logPureAction` to `PureLogging`.
* Add `withSublogger` to `HasLoggerName`.

1.5.1
=====

* Add `usingNamedPureLogger` to `PureLogging`.

1.5.0
=====

* Replace `String` to `Text` in `LoggerName`.
* Rename `LoggerName` field name to `getLoggerName`.
* Rename `getLoggerName` of `HasLoggerName` class to `askLoggerName`.
* Use `LoggerName` instead of `Text` where possible.
* Make separate `HasLoggerName` module.
* Make separate `PureLogging` module.
* Remove `safecopy` dependency and refactor code.

1.4.1
=====

* Add `logEvents` function to log `[LogEvent]` with proper logName.

1.4.0
=====

* Add ability to specify custom logging action.

1.3.4
====

* Correct logger config parsing

1.3.3
=====

* Fixed a bug related to ugly output to stdout even when it was turned off.

1.3.2
=====

* Minor dependencies update.

1.3.1
=====

* Fix minor bug with stdout severity.

1.3.0
=====

* Allow to use arbitrary text formatter function.

1.2.4
=====

* Add ability to specify time format for logs.
* Some space leaks elimination:
  + The `MemoryQueue` has been partially reworked to get rid of the "inline" State manipulation;
  + Strings have been dropped almost everywhere in favour of `Text`;
  + A `LogFormatter` has been reworked to yield a `IO Builder`;
  + `replaceVarM` has been reworked to be pure _and_ to work with builders rather than plain Text/Strings;
  + The pure logger has been reworked to use strict's `StateT` instead of WriterT;
  + The pure logger have been polished to drop instances which required the `UndecidableInstances` pragma;
  + The `Sized` instance for `Text` has been reworked and multiplied by a constant factor of 16 (see below).

1.2.3
=====

* Now we create a directory for log files if it's missing.

1.2.2
=====

* Fixed memory leak (PR #17).

1.2.1
=====

* Supports Unix paths in log configs even on Windows.

1.2.0
=====

* Uses universum-0.6.1.

1.1.4
=====

* Add `CanLog` and `HasLoggerName` instances for both strict and lazy `State`.

1.1.3
=====

* Add config parameter to print `ThreadId` optionally.
* Boolean monoidal builders for `LoggerConfig` now set boolean
  parameter to default ≠ `mempty` parameter.
