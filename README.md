# log-warper

Auxilary logging library, which is wrapper over
[hslogger](http://hackage.haskell.org/package/hslogger) but allows
to keep logger name into monadic context, making logging less boilerplate.

Key features:

1. Output is colored :star:
2. Supports logging initialization from `.yaml` configuration file.
3. Has wrapper for pure logging via `WriterT`.
