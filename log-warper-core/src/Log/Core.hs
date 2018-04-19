{- | Reexports core functions for @log-warper@ library. Core of @log-warper@ contains two parts:

1. 'LogAction' data type which describes how to log things.
2. 'MonadLogger' type class which describes how to get 'LogAction' from context.
-}

module Log.Core
       ( module Log.Core.Action
       , module Log.Core.Class
       ) where

import Log.Core.Action
import Log.Core.Class
