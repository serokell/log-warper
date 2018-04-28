-- | Actions for printing logging records to terminal.

module Log.Actions.Terminal
       ( -- * Console printing actions
         logEventStdout
       , logTextStdout
       ) where

import Universum

import Log.Core.Action (LogAction (..), cmap)
import Log.Event (Event, makeRich)
import Log.Fmt (richEventF)

-- | Very simple action which just prints 'Text' to @stdout@.
logTextStdout :: MonadIO m => LogAction m Text
logTextStdout = LogAction putTextLn

-- | Simple action which prints every incoming event into @stdout@.
logEventStdout :: MonadIO m => LogAction m (Event exts)
logEventStdout = makeRich $ cmap richEventF logTextStdout
