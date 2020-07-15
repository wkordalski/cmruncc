{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module Main where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Base16 (decode)
import qualified Data.Text as T
import Network.Socket
import System.IO.Temp
import System.Posix.Pty
import System.Process
import System.Timeout

import CMRunCC.Network (resolve, handle, send)
import CMRunCC.Messages (RunRequest (..), RunResults (..))
import Config ( readConfig, RunnerConfig (..) )
import Pty

type RunQueue = Chan RunRequest
type ResultsQueue = Chan RunResults

main :: IO ()
main = do
    RunnerConfig { devices, heni_cmd, temp_dir, scheduler_address } <- readConfig
    run_queue <- newChan
    results_queue <- newChan
    forM devices (\d -> forkIO $ runner heni_cmd temp_dir d run_queue results_queue)
    addr <- resolve scheduler_address "4244"
    E.bracket (open addr) close (communication run_queue results_queue)
    where
        open addr = do
            sock <- socket AF_INET Stream 0
            setSocketOption sock ReuseAddr 1
            connect sock $ addrAddress addr
            return sock

communication run_queue results_queue sock = do
    forkIO $ sender results_queue sock
    handle (received run_queue) sock
    return ()

received :: RunQueue -> RunRequest -> IO ()
received run_queue rr = do
    putStrLn "Received request"
    writeChan run_queue rr

sender results_queue sock = forever $ do
    msg <- readChan results_queue
    putStrLn "Sending response"
    send sock msg


-- Runner thread
runner :: String -> String -> String -> RunQueue -> ResultsQueue -> IO ()
runner heni_cmd temp_dir device run_queue results_queue = do
    E.bracket
        (spawnWithPty Nothing False heni_cmd ["n", "u", "l", device] (80, 60) >>= \(pty, ph) -> makeBPty pty ph)
        (\bpty -> killBPty bpty >> putStrLn "Killing HENI NUL")
        (\bpty -> do
            res <- timeout (120*1000000) $ waitForMessage bpty (BS.pack "Terminal ready")
            case res of
                Nothing -> do
                    putStrLn "Failed connecting to the terminal!"
                Just () -> do 
                    putStrLn "Terminal is ready!"
                    run_hex_loop bpty heni_cmd temp_dir device run_queue results_queue
        )
    runner heni_cmd temp_dir device run_queue results_queue

run_hex_loop bpty heni_cmd temp_dir device run_queue results_queue = forever $ do
    req <- readChan run_queue
    let RunRequest { ident } = req
    res <- runExceptT $ run_hex bpty heni_cmd temp_dir device req
    case res of
        Left () -> writeChan results_queue $ RunResults { ident, symbols = [] } -- TODO
        Right v -> writeChan results_queue v

run_hex :: BPty -> String -> String -> String -> RunRequest -> ExceptT () IO RunResults
run_hex bpty heni_cmd temp_dir device req = do
    let RunRequest { ident, hex, symbols } = req
    if BS.null hex then throwError () else return ()
    withTempFile temp_dir "flash-.hex" $ \fn h -> do
        liftIO $ BS.hPut h hex
        retry 2 $ timeoutE (40*1000000) () $ program_and_read bpty heni_cmd fn device req


program_and_read :: BPty -> String -> String -> String -> RunRequest -> ExceptT () IO RunResults
program_and_read bpty heni_cmd hex_file device req = do
    let RunRequest { ident, hex, symbols } = req

    (_, _, _, ph) <- liftIO $ createProcess (proc heni_cmd ["n", "pr", "cherry", "-d", "dev:"++device, hex_file])
    rc <- liftIO $ waitForProcess ph

    liftIO $ waitForMessage bpty (BS.pack $ "Benchmark ID: " ++ ident)
    liftIO $ waitForMessage bpty (BS.pack "\n")      -- after Benchmark ID
    liftIO $ waitForMessage bpty (BS.pack "\n")      -- after times
    liftIO $ waitForMessage bpty (BS.pack "\n")      -- after results

    let symbols_count = length symbols
    res <- replicateM symbols_count $ (liftIO $ takeOutputTillMessage bpty (BS.pack "\n")) >>= parse_symbol

    return $ RunResults { ident, symbols = res }

parse_symbol :: BS.ByteString -> ExceptT () IO (String, Int, BS.ByteString)
parse_symbol bs = do
    let (name_bs, bs1) = BS.break (== ':') bs
    let name = BS.unpack name_bs
    let bs2 = BS.drop 7 bs1
    let (addr_str, bs3) = BS.break (== ',') bs2
    let bs4 = BS.drop 6 bs3
    let (data1, bs5) = BS.breakSubstring (BS.pack " end") bs4
    let data2 = BS.filter (/= ' ') data1
    let (data_str, rest) = decode data2
    return (name, read (BS.unpack addr_str), data_str)

retry :: Int -> ExceptT e IO a -> ExceptT e IO a
retry 0 m = m
retry n m = do
    m `catchError` \_ -> retry (n-1) m

timeoutE :: Int -> e -> ExceptT e IO a -> ExceptT e IO a
timeoutE t e m = do
    res <- liftIO $ timeout t $ runExceptT m
    case res of
        Nothing -> throwError e
        Just (Left e) -> throwError e
        Just (Right a) -> return a
