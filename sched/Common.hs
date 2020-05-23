module Common (BuildQueue, locking) where

import Control.Concurrent
import CMRunCC.Messages (PublicAPIRequest)

type BuildQueue = Chan PublicAPIRequest

locking :: IO a -> MVar () -> IO a
a `locking` l = takeMVar l >> (a <* putMVar l ())