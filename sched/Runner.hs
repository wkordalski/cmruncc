{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module Runner (handleRunnerClient) where

import Control.Concurrent
import Data.IORef
import qualified Data.Set as S
import Network.Socket

import Common (RunQueue, ResponseQueue, BuildResultsStorage, takeResultFromStorage, locking)
import CMRunCC.Messages (BuildResults (..), RunRequest (..), RunResults (..), PublicAPIResponse (..))
import CMRunCC.Network (handle, send)

data Runner = Runner {
    lock :: MVar (),
    wait :: MVar (),
    requests :: IORef (S.Set String)
}

locked :: Runner -> IO a -> IO a
locked b f = let Runner { lock } = b in f `locking` lock

add :: Runner -> String -> IO ()
add b n = locked b $ do
    reqs <- readIORef $ requests b
    let new_reqs = S.insert n reqs
    if S.size new_reqs > 16 then takeMVar (wait b) else return ()
    writeIORef (requests b) new_reqs

done :: Runner -> String -> IO ()
done b n = locked b $ do
    reqs <- readIORef $ requests b
    let new_reqs = S.delete n reqs
    if S.size new_reqs < 16 then tryPutMVar (wait b) () >> return () else return ()
    writeIORef (requests b) new_reqs


handleRunnerClient :: RunQueue -> ResponseQueue -> BuildResultsStorage -> Socket -> IO ()
handleRunnerClient run_queue response_queue storage sock = do
    requests <- newIORef $ S.empty
    lock <- newMVar ()
    wait <- newMVar ()
    let runner = Runner { lock, wait, requests }
    feeder_thread <- forkIO $ runnerFeeder sock runner run_queue
    handle (runnerAPI runner response_queue storage) sock
    killThread feeder_thread
    reqs <- locked runner $ do
        reqs <- readIORef $ requests
        return $ S.toList reqs
    -- TODO: browse reqs and add requests to the run queue again
    return ()

-- Process received messages
-- TODO: needs RunQueue, BuildInfo, Requests
-- TODO: get BuildResult instead of String
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
    add runner ident
    -- if runner is well-fed, wait for some request done
    takeMVar $ wait runner
    putMVar (wait runner) ()
    runnerFeeder sock runner run_queue
