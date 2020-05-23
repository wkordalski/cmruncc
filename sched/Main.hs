{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module Main where

import CMRunCC.Messages (RunRequest (..), PublicAPIRequest (..), PublicAPIResponse (..))
import CMRunCC.Network (resolve, server, handle, send)
import qualified Data.ByteString as B
import qualified Data.Map.Lazy as M
import Data.IORef
import Control.Concurrent
import Network.Socket (Socket)
import Builder (handleBuilderClient)
import Common (BuildQueue, locking)

data Client = Client {
    socket :: Socket
}

type Requests = (MVar (), IORef (M.Map String Client))

initClients :: IO Requests
initClients = do
    lock <- newMVar ()
    map <- newIORef $ M.empty
    return (lock, map)

withClients :: Requests -> (IORef (M.Map String Client) -> IO a) -> IO a
withClients c f = do
    let (lock, map) = c
    takeMVar lock
    res <- f map
    putMVar lock ()
    return res

addIdentifierForClient :: String -> Client -> Requests -> IO ()
addIdentifierForClient k v c = withClients c $ \m -> do
    map <- readIORef m
    writeIORef m $ M.insert k v map




-- Global state:
--  build_results: id -> build result { flash + addresses }
--  run_queries: [run query]

main :: IO ()
main = do
    clients <- initClients
    buildQueue <- newChan

    addrB <- resolve "0.0.0.0" "4243"
    forkIO $ server addrB $ handleBuilderClient buildQueue

    addrR <- resolve "0.0.0.0" "4244"
    forkIO $ server addrR $ handleRunnerClient

    addr <- resolve "0.0.0.0" "4242"
    server addr $ handlePublicAPIClient clients buildQueue

handlePublicAPIClient :: Requests -> BuildQueue -> Socket -> IO ()
handlePublicAPIClient clients build_queue sock = do
    let c = Client { socket = sock }
    handle (publicAPI c clients build_queue) sock
    return ()

publicAPI :: Client -> Requests -> BuildQueue -> PublicAPIRequest -> IO ()
publicAPI c clients build_queue r = do
    let PublicAPIRequest { ident } = r
    addIdentifierForClient ident c clients
    writeChan build_queue r
    withClients clients $ \_ -> do
        send (socket c) $ PublicAPIResponse {
                ident = ident,
                flash = B.pack [0, 1, 2, 3],
                emulator_main_addr = 1,
                emulator_cdl_start_addr = 2,
                emulator_exit_addr = 3,
                mem_dump = [("times", 53, B.pack [0, 0, 0, 1])]
            }


handleRunnerClient :: Socket -> IO ()
handleRunnerClient sock = do
    handle (runnerAPI) sock
    return ()

runnerAPI :: Int -> IO ()
runnerAPI r = do
    putStrLn $ show r