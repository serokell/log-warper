{- | Reexports core functions for @log-warper@ library. Core of @log-warper@ contains two parts:

1. 'LogAction' data type which describes how to log things.
2. 'MonadLogger' type class which describes how to get 'LogAction' from context.
-}

module Logw.Core
       ( module Logw.Core.Action
       , module Logw.Core.Class
       ) where

import Logw.Core.Action
import Logw.Core.Class
