{-# LANGUAGE NamedFieldPuns #-}

module Pty (BPty, makeBPty, killBPty, waitForMessage, takeOutputTillMessage) where

import System.Posix.Pty
import System.Process
import Control.Concurrent
import Control.Monad
import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Control.Exception as E

data BPty = BPty {
    chan :: Chan BS.ByteString,
    buf :: IORef BS.ByteString,
    ph :: ProcessHandle,
    rth :: ThreadId
}

makeBPty :: Pty -> ProcessHandle -> IO BPty
makeBPty pty ph = do
    chan <- newChan
    rth <- forkFinally (ptyThread chan pty) (\_ -> terminateProcess ph >> closePty pty)
    buf <- newIORef $ BS.empty
    return $ BPty { chan=chan, buf=buf, ph=ph, rth=rth }

ptyThread :: Chan BS.ByteString -> Pty -> IO ()
ptyThread ch pty = forever $ do
    bs <- readPty pty
    let bs' = BS.filter (/= fromIntegral 0) bs
    if BS.length bs' > 0 then
        writeChan ch bs'
    else
        return ()

killBPty :: BPty -> IO ()
killBPty bpty = do
    let BPty { rth } = bpty
    killThread rth          -- stop reader thread

feed :: BPty -> IO ()
feed bpty = do
    let BPty { chan, buf } = bpty
    bs <- readChan chan
    --BSC.putStrLn bs
    b <- readIORef buf
    writeIORef buf $ BS.append b bs

takeOutputTillMessage :: BPty -> BS.ByteString -> IO BS.ByteString
takeOutputTillMessage bpty pattern = do
    let BPty { chan, buf } = bpty

    let pattLen = BS.length pattern
    let breakPattern = BS.breakSubstring pattern
    
    b <- readIORef buf
    let (h, t) = breakPattern b
    if BS.length t == 0 then do
        feed bpty
        takeOutputTillMessage bpty pattern
    else do
        let bd = BS.drop pattLen t
        writeIORef buf bd
        return h

waitForMessage :: BPty -> BS.ByteString -> IO ()
waitForMessage bpty pattern = do
    takeOutputTillMessage bpty pattern >> return ()