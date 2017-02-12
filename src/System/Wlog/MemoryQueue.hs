{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Queue for in-memory logs. Rolls out old logging if size of queue is bigger than
-- predefined limit.

module System.Wlog.MemoryQueue
       ( MemoryQueue (..)
       , dumpQueueMessages
       , newMemoryQueue
       , popLast
       , pushFront
       ) where

import           Universum

import           Control.Lens        (makeLenses, (%=), (+=))
import           Control.Monad.Loops (whileM_)

import           Data.Sequence       (Seq, ViewR (..), viewr, (<|))

-- | Data structure similar to queue but pops out elements after 'pushFront'
-- if 'mqMemSize' > 'mqLimit'. Stores only 'Text' because intended to use only
-- with log messages and it's not clear how to measure /memory size/ in general case.
data MemoryQueue = MemoryQueue
    { _mqLimit   :: !Word64
    , _mqMemSize :: !Word64
    , _mqQueue   :: !(Seq Text)
    }

makeLenses ''MemoryQueue

-- | Creates new memory queue.
newMemoryQueue :: Word64 -> MemoryQueue
newMemoryQueue _mqLimit = MemoryQueue { _mqMemSize = 0, _mqQueue = mempty, .. }

-- | Removes last element from 'MemoryQueue'.
popLast :: MemoryQueue -> (Maybe Text, MemoryQueue)
popLast mq@MemoryQueue{..} = case viewr _mqQueue of
    EmptyR         -> (Nothing, mq)
    rest :> popped -> let newMemSize = _mqMemSize - fromIntegral (length popped)
                      in (Just popped, MemoryQueue{ _mqMemSize = newMemSize, _mqQueue = rest, .. })

-- | Add new element at the beginning removing elements from the end
-- untill size become not greater than limit.
pushFront :: Text -> MemoryQueue -> MemoryQueue
pushFront msg mq = executingState mq $ do
    mqMemSize += fromIntegral (length msg)
    mqQueue   %= (msg <|)
    whileM_ isLimitExceeded $
        modify (snd . popLast)
  where
    isLimitExceeded = liftA2 (<) (use mqLimit) (use mqMemSize)

-- | Converts queue to list of messages.
dumpQueueMessages :: MemoryQueue -> [Text]
dumpQueueMessages MemoryQueue{..} = toList _mqQueue
