{-# LANGUAGE TemplateHaskell #-}

-- | Queue for in-memory logs. Rolls out old logging if size of queue
-- is bigger than predefined limit.

module System.Wlog.MemoryQueue
       ( Sized(..)
       , MemoryQueue(..)
       , newMemoryQueue
       , popLast
       , pushFront
       , queueToList
       -- * Useful lenses
       , mqMemSize
       , mqLimit
       ) where

import Universum

import Data.Sequence (Seq, ViewR (..), viewr, (<|))
import Lens.Micro.Platform (makeLenses)

import qualified Data.Text as T

-- | Class for objects that have size. Implementations can take size
-- as amount of memory items take, as amount of items in container,
-- etc.
class Sized e where
    getSize :: e -> Word64

-- Instance for text size that pessimistically multiply the number
-- of characters by 16, although the char-varying nature of UTF16
-- means this is greater or equal the true size in bytes.
instance Sized Text where
    getSize = fromIntegral . (* 16) . T.length

-- | Data structure similar to queue but pops out elements after
-- 'pushFront' if 'mqMemSize' > 'mqLimit'.
data MemoryQueue a = MemoryQueue
    { _mqLimit   :: !Word64
    , _mqMemSize :: !Word64
    , _mqQueue   :: !(Seq a)
    } deriving (Show)

makeLenses ''MemoryQueue

-- | Creates new memory queue.
newMemoryQueue :: (Sized a) => Word64 -> MemoryQueue a
newMemoryQueue _mqLimit = MemoryQueue { _mqMemSize = 0, _mqQueue = mempty, .. }

-- | Removes last element from 'MemoryQueue'.
popLast :: (Sized a) => MemoryQueue a -> (Maybe a, MemoryQueue a)
popLast mq@MemoryQueue{..} = case viewr _mqQueue of
    EmptyR         -> (Nothing, mq)
    rest :> popped ->
        let newMemSize = _mqMemSize - getSize popped
        in (Just popped, MemoryQueue{ _mqMemSize = newMemSize, _mqQueue = rest, .. })

-- | Add new element at the beginning removing elements from the end
-- until size become not greater than limit.
pushFront :: (Sized a) => a -> MemoryQueue a -> MemoryQueue a
pushFront msg oldQueue =
  let msgSize = getSize msg
      newSize = _mqMemSize oldQueue + msgSize
  in resize oldQueue {
      _mqMemSize = newSize
    , _mqQueue   = msg <| _mqQueue oldQueue
    }
  where
    -- Resize the queue so that the inner @Seq@ won't contain more than mqLimit
    -- elements.
    resize :: Sized a => MemoryQueue a -> MemoryQueue a
    resize theQueue = case _mqMemSize theQueue > _mqLimit theQueue of
        False -> theQueue
        True  -> let (_, q') = popLast theQueue in resize $! q'

-- | Converts queue to list of messages.
queueToList :: (Sized a) => MemoryQueue a -> [a]
queueToList = toList . view mqQueue
