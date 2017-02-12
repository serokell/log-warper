{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Queue for in-memory logs. Rolls out old logging if size of queue
-- is bigger than predefined limit.

module System.Wlog.MemoryQueue
       ( Sized(..)
       , MemoryQueue(..)
       , newMemoryQueue
       , popLast
       , pushFront
       , toList
       ) where

import           Control.Lens        (to)
import           Data.Sequence       (Seq, ViewR (..), viewr, (<|))
import           Universum           hiding (toList)
import qualified Universum           as U

import           Control.Lens        (makeLenses, (%=), (+=))
import           Control.Monad.Loops (whileM_)

-- | Class for objects that have size. Implementations can take size
-- as amount of memory items take, as amount of items in container,
-- etc.
class Sized e where
    getSize :: e -> Word64

-- Instance for text size that takes number of chars in text as size
-- (not actual bytes).
instance Sized Text where
    getSize = fromIntegral . length


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
-- untill size become not greater than limit.
pushFront :: (Sized a) => a -> MemoryQueue a -> MemoryQueue a
pushFront msg mq = executingState mq $ do
    mqMemSize += getSize msg
    mqQueue   %= (msg <|)
    whileM_ isLimitExceeded $
        modify (snd . popLast)
  where
    isLimitExceeded = liftA2 (<) (use mqLimit) (use mqMemSize)

-- | Converts queue to list of messages.
toList :: (Sized a) => MemoryQueue a -> [a]
toList = view $ mqQueue . to U.toList
