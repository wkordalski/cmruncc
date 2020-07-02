{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module Builder (handleBuilderClient) where

import Control.Concurrent
import Control.Monad
import qualified Control.Exception as E
import Data.IORef
import qualified Data.Map as M
import Network.Socket

import Common (BuildQueue, RunQueue, BuildResultsStorage, insertResultIntoStorage, locking)
import CMRunCC.Messages (PublicAPIRequest (..), BuildResults (..), RunRequest (..))
import CMRunCC.Network (handle, send)

data Builder = Builder {
    lock :: MVar (),
    wait :: MVar (),
    requests :: IORef (M.Map String PublicAPIRequest)
}

locked :: Builder -> IO a -> IO a
locked b f = let Builder { lock } = b in f `locking` lock

add :: Builder -> String -> PublicAPIRequest -> IO ()
add b n r = locked b $ do
    reqs <- readIORef $ requests b
    let new_reqs = M.insert n r reqs
    if M.size new_reqs > 16 then takeMVar (wait b) else return ()
    writeIORef (requests b) new_reqs

done :: Builder -> String -> IO ()
done b n = locked b $ do
    reqs <- readIORef $ requests b
    let new_reqs = M.delete n reqs
    if M.size new_reqs < 16 then tryPutMVar (wait b) () >> return () else return ()
    writeIORef (requests b) new_reqs


handleBuilderClient :: BuildQueue -> RunQueue -> BuildResultsStorage -> Socket -> IO ()
handleBuilderClient build_queue run_queue storage sock = do
    requests <- newIORef $ M.empty
    lock <- newMVar ()
    wait <- newMVar ()
    let builder = Builder { lock, wait, requests }
    feeder_thread <- forkIO $ builderFeeder sock builder build_queue
    E.finally (handle (builderAPI builder run_queue storage) sock)
        (do
            killThread feeder_thread
            reqs <- locked builder $ do
                reqs <- readIORef $ requests
                return $ M.toList reqs
            forM_ reqs (\(_, req) -> writeChan build_queue req)
        )

-- Process received messages
builderAPI :: Builder -> RunQueue -> BuildResultsStorage -> BuildResults -> IO ()
builderAPI builder run_queue storage r = do
    putStrLn "Got builder response"
    let BuildResults { ident, emulator_main_addr, emulator_exit_addr, emulator_cdl_start_addr, symbols, hex } = r
    done builder ident
    insertResultIntoStorage storage r
    writeChan run_queue $ RunRequest { hex, ident, symbols }

-- Send messages
builderFeeder :: Socket -> Builder -> BuildQueue -> IO ()
builderFeeder sock builder build_queue = do
    -- get next request
    rq <- readChan build_queue
    let PublicAPIRequest {ident} = rq
    -- send request to builder
    E.onException (send sock rq) (writeChan build_queue rq)
    add builder ident rq
    -- if builder is well-fed, wait for some request done
    takeMVar $ wait builder
    putMVar (wait builder) ()
    builderFeeder sock builder build_queue
