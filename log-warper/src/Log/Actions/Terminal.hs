-- | Actions for printing logging records to terminal.

module Log.Actions.Terminal
       ( -- * Console printing actions
         logPrint
       , logStdout
       ) where

import Universum

import Log.Core.Action (LogAction (..), cmap)
import Log.Event (Event, makeRich)
import Log.Fmt (richEventF)

-- | Very simple action which just prints 'Text' to @stdout@.
logPrint :: MonadIO m => LogAction m Text
logPrint = LogAction putTextLn

-- | Simple action which prints every incoming event into @stdout@.
logStdout :: MonadIO m => LogAction m (Event exts)
logStdout = makeRich $ cmap richEventF logPrint
