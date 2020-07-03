{-# LANGUAGE NamedFieldPuns #-}

module Common (
        BuildQueue, RunQueue, ResponseQueue,
        BuildResultsStorage,
        WorkerTracker, handleWorker,
        initBuildResultsStorage, insertResultIntoStorage, takeResultFromStorage,
        locking
    ) where

import CMRunCC.Messages (PublicAPIRequest, PublicAPIResponse, BuildResults (..), RunRequest)
import CMRunCC.Network (handle, send)

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Data.IORef
import qualified Data.Map as M
import qualified Data.MessagePack as MP
import Network.Socket

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

-- WorkerTracker - tracks responses from workers and sends next requests if one has not enough

data WorkerTracker a = WorkerTracker {
    lock :: MVar (),                     -- so that many threads can access this structure
    wait :: MVar (),                     -- so that feeder thread can hang when the worker has enough requests provided
    requests :: IORef (M.Map String a)   -- requests sent to the worker
}

lockedWorker :: WorkerTracker a -> IO b -> IO b
lockedWorker w m = m `locking` (lock w)

add :: WorkerTracker a -> String -> a -> IO ()
add w n r = do
    new_reqs_size <- lockedWorker w $ do
        reqs <- readIORef $ requests w
        let new_reqs = M.insert n r reqs
        writeIORef (requests w) new_reqs
        return $ M.size new_reqs
    if new_reqs_size > 16 then takeMVar (wait w) else return ()

done :: WorkerTracker a -> String -> IO ()
done w n = do
    new_reqs_size <- lockedWorker w $ do
        reqs <- readIORef $ requests w
        let new_reqs = M.delete n reqs
        writeIORef (requests w) new_reqs
        return $ M.size new_reqs
    if new_reqs_size < 16 then tryPutMVar (wait w) () >> return () else return ()
    

handleWorker :: MP.MessagePack a => MP.MessagePack b => (a -> String) -> (b -> String) -> Chan a -> s -> (s -> b -> IO ()) -> Socket -> IO ()
handleWorker input_ident output_ident input_queue output on_response sock = do
    lock <- newMVar ()
    wait <- newMVar ()
    requests <- newIORef $ M.empty
    let worker = WorkerTracker { lock, wait, requests }
    feeder_thread <- forkIO $ feederThread input_ident sock input_queue worker
    E.finally (handle (\r -> done worker (output_ident r) >> on_response output r) sock)
        (do
            killThread feeder_thread
            reqs <- lockedWorker worker $ do
                reqs <- readIORef $ requests
                return $ M.toList reqs
            forM_ reqs (\(_, req) -> writeChan input_queue req)
        )

feederThread :: MP.MessagePack a => (a -> String) -> Socket -> Chan a -> WorkerTracker a -> IO ()
feederThread ident sock queue tracker = forever $ do
    -- get next request
    rq <- readChan queue
    let request_id = ident rq
    -- send request to the worker
    E.onException (send sock rq) (writeChan queue rq)
    add tracker request_id rq
    -- if builder is well-fed, wait for some request done
    takeMVar (wait tracker)
    putMVar (wait tracker) ()