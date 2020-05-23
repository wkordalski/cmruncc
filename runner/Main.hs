module Main where

import Network.Socket
import CMRunCC.Network (resolve, handle)
import CMRunCC.Messages (RunRequest (..))
import qualified Control.Exception as E

main :: IO ()
main = do
    addr <- resolve "127.0.0.1" "4244"
    E.bracket (open addr) close (handle msgH)
    where
        open addr = do
            sock <- socket AF_INET Stream 0
            setSocketOption sock ReuseAddr 1
            connect sock $ addrAddress addr
            return sock

msgH :: RunRequest -> IO ()
msgH rr = putStrLn $ show rr