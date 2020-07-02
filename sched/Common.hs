{-# LANGUAGE NamedFieldPuns #-}

module Common (
        BuildQueue, RunQueue, ResponseQueue,
        BuildResultsStorage,
        initBuildResultsStorage, insertResultIntoStorage, takeResultFromStorage,
        locking
    ) where

import Control.Concurrent
import Data.IORef
import CMRunCC.Messages (PublicAPIRequest, PublicAPIResponse, BuildResults (..), RunRequest)
import qualified Data.Map as M

type BuildQueue = Chan PublicAPIRequest
type RunQueue = Chan RunRequest
type ResponseQueue = Chan PublicAPIResponse

locking :: IO a -> MVar () -> IO a
a `locking` l = takeMVar l >> (a <* putMVar l ())


-- BuildResultsStorage

type BuildResultsStorage = (MVar (), IORef (M.Map String BuildResults))

initBuildResultsStorage :: IO BuildResultsStorage
initBuildResultsStorage = do
    lock <- newMVar ()
    map <- newIORef $ M.empty
    return (lock, map)

insertResultIntoStorage :: BuildResultsStorage -> BuildResults -> IO ()
insertResultIntoStorage storage result = do
    let BuildResults { ident } = result
    let (lock, map) = storage
    (readIORef map >>= \m -> writeIORef map $ M.insert ident result m) `locking` lock
    return ()

takeResultFromStorage :: BuildResultsStorage -> String -> IO BuildResults
takeResultFromStorage storage ident = do
    let (lock, map) = storage
    (readIORef map >>= \m -> writeIORef map (M.delete ident m) >> return (m M.! ident)) `locking` lock
