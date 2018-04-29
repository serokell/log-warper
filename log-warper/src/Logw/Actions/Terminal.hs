-- | Actions for printing logging records to terminal.

module Logw.Actions.Terminal
       ( -- * Console printing actions
         logEventStdout
       , logTextStdout
       ) where

import Universum

import Logw.Core.Action (LogAction (..), cmap)
import Logw.Event (Event, makeRich)
import Logw.Fmt (richEventF)

-- | Very simple action which just prints 'Text' to @stdout@.
logTextStdout :: MonadIO m => LogAction m Text
logTextStdout = LogAction putTextLn

-- | Simple action which prints every incoming event into @stdout@.
logEventStdout :: MonadIO m => LogAction m (Event exts)
logEventStdout = makeRich $ cmap richEventF logTextStdout
