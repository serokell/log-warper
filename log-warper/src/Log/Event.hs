{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Contains implementations of logging records and events which are consumed by 'LogAction'.

module Log.Event
       ( -- * Types
         Event (..)
       , eName
       , eSeverity
       , eMessage
       , eConfiguration

       , RichEvent (..)
       , reEvent
       , reThread
       , reTime

         -- * Functions
       , makeRich
       ) where

import Universum

import Control.Concurrent (ThreadId, myThreadId)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Lens.Micro.Platform (makeLenses)

import Log.Configuration (Configuration, Extension, cGlobal, gpShowTid, gpShowTime)
import Log.Core.Action (LogAction (..), cbind)
import Log.Name (LoggerName (..))
import Log.Severity (Severity (..))


{- | Bundle of all basic logging data. This logging record also contains
configuration for logging. It can be used to process messages. All 'LogAction's
in @log-warper@ should have form:

@
'LogAction' someM ('Event' exts)
@
-}
data Event (exts :: [Extension]) = Event
    { _eName          :: !LoggerName
    , _eSeverity      :: !Severity
    , _eMessage       :: !Text
    , _eConfiguration :: !(Configuration exts)
    }

makeLenses ''Event

-- | Logging record which contains 'Event' and also other things accessible
-- inside 'IO'.
data RichEvent exts = RichEvent
    { _reEvent  :: !(Event exts)
    , _reThread :: !(Maybe ThreadId)
    , _reTime   :: !(Maybe UTCTime)
    }

makeLenses ''RichEvent

maybeGuard :: Applicative f => Bool -> f a -> f (Maybe a)
maybeGuard False _ = pure Nothing
maybeGuard True  f = Just <$> f

-- | Allows to consume 'RichEvent' by reading current time and thread id from 'IO'.
makeRich :: MonadIO m => LogAction m (RichEvent exts) -> LogAction m (Event exts)
makeRich = cbind (liftIO . toRich)
  where
    toRich :: Event exts -> IO (RichEvent exts)
    toRich event = do
        let global = event ^. eConfiguration . cGlobal
        time <- maybeGuard (global ^. gpShowTime) getCurrentTime
        tid  <- maybeGuard (global ^. gpShowTid)  myThreadId
        pure $ RichEvent event tid time
