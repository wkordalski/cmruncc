{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module Main where

import CMRunCC.Messages (RunRequest (..), PublicAPIRequest (..), PublicAPIResponse (..))
import CMRunCC.Network (resolve, server, handle, send)

import Common (BuildQueue, ResponseQueue, BuildResultsStorage, initBuildResultsStorage, locking)
import Config (SchedConfig (..), readConfig)
import Builder (handleBuilderClient)
import Runner (handleRunnerClient)

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Map.Lazy as M
import Network.Socket (Socket)


data Client = Client {
    socket :: Socket
}

type Clients = (MVar (), IORef (M.Map String Client))

initClients :: IO Clients
initClients = do
    lock <- newMVar ()
    map <- newIORef $ M.empty
    return (lock, map)

withClients :: Clients -> (IORef (M.Map String Client) -> IO a) -> IO a
withClients c f = do
    let (lock, map) = c
    f map `locking` lock

addIdentifierForClient :: String -> Client -> Clients -> IO ()
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

handlePublicAPIClient :: Clients -> BuildQueue -> Socket -> IO ()
handlePublicAPIClient clients build_queue sock = do
    let c = Client { socket = sock }
    handle (publicAPI c clients build_queue) sock

publicAPI :: Client -> Clients -> BuildQueue -> PublicAPIRequest -> IO ()
publicAPI client clients build_queue request = do
    let PublicAPIRequest { ident } = request
    addIdentifierForClient ident client clients
    writeChan build_queue request

responseThread :: ResponseQueue -> Clients -> IO ()
responseThread response_queue clients = forever $ do
    resp <- readChan response_queue
    let PublicAPIResponse { ident } = resp
    withClients clients $ \map_ref -> do
        map <- readIORef map_ref
        send (socket $ map M.! ident) resp
        writeIORef map_ref $ M.delete ident map
