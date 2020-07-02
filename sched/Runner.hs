{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module Runner (handleRunnerClient) where

import Control.Concurrent
import Control.Monad
import Data.IORef
import qualified Data.Map as M
import Network.Socket

import Common (RunQueue, ResponseQueue, BuildResultsStorage, takeResultFromStorage, locking)
import CMRunCC.Messages (BuildResults (..), RunRequest (..), RunResults (..), PublicAPIResponse (..))
import CMRunCC.Network (handle, send)

-- Store map: String -> RunRequest so after runner shutdown the requests may be restarted on another one
data Runner = Runner {
    lock :: MVar (),
    wait :: MVar (),
    requests :: IORef (M.Map String RunRequest)
}

locked :: Runner -> IO a -> IO a
locked b f = let Runner { lock } = b in f `locking` lock

add :: Runner -> String -> RunRequest -> IO ()
add b n r = locked b $ do
    reqs <- readIORef $ requests b
    let new_reqs = M.insert n r reqs
    if M.size new_reqs > 16 then takeMVar (wait b) else return ()
    writeIORef (requests b) new_reqs

done :: Runner -> String -> IO ()
done b n = locked b $ do
    reqs <- readIORef $ requests b
    let new_reqs = M.delete n reqs
    if M.size new_reqs < 16 then tryPutMVar (wait b) () >> return () else return ()
    writeIORef (requests b) new_reqs


handleRunnerClient :: RunQueue -> ResponseQueue -> BuildResultsStorage -> Socket -> IO ()
handleRunnerClient run_queue response_queue storage sock = do
    requests <- newIORef $ M.empty
    lock <- newMVar ()
    wait <- newMVar ()
    let runner = Runner { lock, wait, requests }
    feeder_thread <- forkIO $ runnerFeeder sock runner run_queue
    handle (runnerAPI runner response_queue storage) sock
    killThread feeder_thread
    reqs <- locked runner $ do
        reqs <- readIORef $ requests
        return $ M.toList reqs
    forM reqs (\(_, req) -> writeChan run_queue req)
    return ()

-- Process received messages
runnerAPI :: Runner -> ResponseQueue -> BuildResultsStorage -> RunResults -> IO ()
runnerAPI runner response_queue storage r = do
    putStrLn "Got runner response"
    let RunResults { ident, symbols } = r
    done runner ident
    BuildResults {
            flash,
            emulator_main_addr, emulator_cdl_start_addr, emulator_exit_addr
        } <- takeResultFromStorage storage ident
    let response = PublicAPIResponse {
            ident, flash,
            emulator_main_addr, emulator_cdl_start_addr, emulator_exit_addr,
            mem_dump=symbols
        }
    writeChan response_queue response
    

-- Send messages
runnerFeeder :: Socket -> Runner -> RunQueue -> IO ()
runnerFeeder sock runner run_queue = do
    -- get next request
    rq <- readChan run_queue
    let RunRequest {ident} = rq
    -- send request to runner
    send sock rq
    add runner ident rq
    -- if runner is well-fed, wait for some request done
    takeMVar $ wait runner
    putMVar (wait runner) ()
    runnerFeeder sock runner run_queue
