{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module Builder (handleBuilderClient) where

import Control.Concurrent
import Data.IORef
import qualified Data.Set as S
import Network.Socket

import Common (BuildQueue, RunQueue, BuildResultsStorage, insertResultIntoStorage, locking)
import CMRunCC.Messages (PublicAPIRequest (..), BuildResults (..), RunRequest (..))
import CMRunCC.Network (handle, send)

data Builder = Builder {
    lock :: MVar (),
    wait :: MVar (),
    requests :: IORef (S.Set String)
}

locked :: Builder -> IO a -> IO a
locked b f = let Builder { lock } = b in f `locking` lock

add :: Builder -> String -> IO ()
add b n = locked b $ do
    reqs <- readIORef $ requests b
    let new_reqs = S.insert n reqs
    if S.size new_reqs > 16 then takeMVar (wait b) else return ()
    writeIORef (requests b) new_reqs

done :: Builder -> String -> IO ()
done b n = locked b $ do
    reqs <- readIORef $ requests b
    let new_reqs = S.delete n reqs
    if S.size new_reqs < 16 then tryPutMVar (wait b) () >> return () else return ()
    writeIORef (requests b) new_reqs


handleBuilderClient :: BuildQueue -> RunQueue -> BuildResultsStorage -> Socket -> IO ()
handleBuilderClient build_queue run_queue storage sock = do
    requests <- newIORef $ S.empty
    lock <- newMVar ()
    wait <- newMVar ()
    let builder = Builder { lock, wait, requests }
    feeder_thread <- forkIO $ builderFeeder sock builder build_queue
    handle (builderAPI builder run_queue storage) sock
    killThread feeder_thread
    reqs <- locked builder $ do
        reqs <- readIORef $ requests
        return $ S.toList reqs
    -- TODO: browse reqs and add requests to the build queue again
    return ()

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
    send sock rq
    add builder ident
    -- if builder is well-fed, wait for some request done
    takeMVar $ wait builder
    putMVar (wait builder) ()
    builderFeeder sock builder build_queue
