{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module Builder (handleBuilderClient) where

import Control.Concurrent
import Data.IORef
import qualified Data.Set as S
import Network.Socket

import Common (BuildQueue, locking)
import CMRunCC.Messages (PublicAPIRequest (..), BuildResults (..))
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


handleBuilderClient :: BuildQueue -> Socket -> IO ()
handleBuilderClient build_queue sock = do
    requests <- newIORef $ S.empty
    lock <- newMVar ()
    wait <- newMVar ()
    let builder = Builder { lock, wait, requests }
    forkIO $ builderFeeder sock builder build_queue
    handle (builderAPI builder) sock
    return ()

-- Process received messages
-- TODO: needs RunQueue, BuildInfo, Requests
-- TODO: get BuildResult instead of String
builderAPI :: Builder -> BuildResults -> IO ()
builderAPI builder r = do
    putStrLn "Got builder response"
    let BuildResults { ident, emulator_main_addr, emulator_exit_addr, emulator_cdl_start_addr, symbols } = r
    done builder ident
    -- TODO: save results and add message to run queue
    putStrLn $ show ident
    putStrLn $ show emulator_main_addr
    putStrLn $ show emulator_cdl_start_addr
    putStrLn $ show emulator_exit_addr
    putStrLn $ show symbols
    putStrLn ""
    

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
