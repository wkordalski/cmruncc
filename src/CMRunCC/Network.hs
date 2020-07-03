module CMRunCC.Network (resolve, server, handle, send) where

import Codec.Compression.Zstd (compress, decompress, Decompress (..))
import Control.Concurrent
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy as BL
import qualified Data.MessagePack as MP
import Network.Socket

import CMRunCC.Messages (readMessage, writeMessage)


resolve host port = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
    head <$> getAddrInfo (Just hints) (Just host) (Just port)

server :: AddrInfo -> (Socket -> IO ()) -> IO ()
server addr handler = do
    E.bracket (open addr) close loop
    where
        open addr = do
            sock <- socket AF_INET Stream 0
            setSocketOption sock ReuseAddr 1
            bind sock $ addrAddress addr
            listen sock 8
            return sock
        loop sock = do
            (s, _) <- accept sock
            forkFinally (handler s) (\e -> close s)
            loop sock

handle :: MP.MessagePack a => (a -> IO ()) -> Socket -> IO ()
handle action sock = do
    mmsg <- readMessage sock
    case mmsg of
        Just msg -> do
            case decompress msg of
                Decompress m ->
                    let obj = MP.unpack (BL.fromStrict m) in
                    case obj of
                        Just o -> action o
                        Nothing -> putStrLn "Invalid message - ignoring"
                Error s -> putStrLn "Decompressing failed - ignoring:" >> putStrLn s
                Skip -> putStrLn "What is this?"
            handle action sock
        Nothing -> return ()

send :: MP.MessagePack a => Socket -> a -> IO ()
send s m = writeMessage s $ compress 7 $ BL.toStrict (MP.pack m)