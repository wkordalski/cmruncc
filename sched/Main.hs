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
import Runner (handleRunnerClient)
import Common (BuildQueue, ResponseQueue, BuildResultsStorage, initBuildResultsStorage, locking)
import Control.Monad
import Config (SchedConfig (..), readConfig)

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
    f map `locking` lock

addIdentifierForClient :: String -> Client -> Requests -> IO ()
addIdentifierForClient k v c = withClients c $ \m -> do
    map <- readIORef m
    writeIORef m $ M.insert k v map

main :: IO ()
main = do
    SchedConfig { listen_address } <- readConfig

    clients <- initClients
    buildQueue <- newChan
    runQueue <- newChan
    responseQueue <- newChan
    buildStorage <- initBuildResultsStorage

    addrB <- resolve listen_address "4243"
    forkIO $ server addrB $ handleBuilderClient buildQueue runQueue buildStorage

    addrR <- resolve listen_address "4244"
    forkIO $ server addrR $ handleRunnerClient runQueue responseQueue buildStorage

    forkIO $ responseThread responseQueue clients

    addr <- resolve listen_address "4242"
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

responseThread :: ResponseQueue -> Requests -> IO ()
responseThread response_queue clients = forever $ do
    resp <- readChan response_queue
    let PublicAPIResponse {ident} = resp
    withClients clients $ \m -> do
        map <- readIORef m
        send (socket $ map M.! ident) resp
