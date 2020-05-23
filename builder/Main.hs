{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module Main where

import Network.Socket
import CMRunCC.Network (resolve, handle, send)
import CMRunCC.Messages (PublicAPIRequest (..), BuildResults (..))
import qualified Control.Exception as E
import Control.Concurrent
import Control.Monad
import System.FilePath
import qualified Data.ByteString.Char8 as BS
import System.Process
import qualified Data.Text as T
import qualified Data.Text.IO as TI

import Config ( readConfig, BuilderConfig (..) )

type BuildQueue = Chan PublicAPIRequest
type ResultsQueue = Chan BuildResults

main :: IO ()
main = do
    BuilderConfig { builders } <- readConfig
    build_queue <- newChan
    results_queue <- newChan
    forM builders (\p -> forkIO $ builder p build_queue results_queue)
    addr <- resolve "127.0.0.1" "4243"
    E.bracket (open addr) close (communication build_queue results_queue)
    where
        open addr = do
            sock <- socket AF_INET Stream 0
            setSocketOption sock ReuseAddr 1
            connect sock $ addrAddress addr
            return sock

communication build_queue results_queue sock = do
    forkIO $ sender results_queue sock
    handle (received build_queue) sock
    return ()


received :: BuildQueue -> PublicAPIRequest -> IO ()
received build_queue rr = do
    putStrLn "Received request"
    writeChan build_queue rr

sender results_queue sock = forever $ do
    msg <- readChan results_queue
    putStrLn "Sending response"
    send sock msg

builder :: FilePath -> BuildQueue -> ResultsQueue -> IO ()
builder path build_queue results_queue = forever $ do
    req <- readChan build_queue
    res <- build path req
    writeChan results_queue res

build :: FilePath -> PublicAPIRequest -> IO BuildResults
build path req = do
    putStrLn ("Building at: " ++ show path)
    let PublicAPIRequest { ident, build_spec, sources, symbols } = req

    -- write build.spec
    BS.writeFile (path </> "build.spec") build_spec

    -- precompile to generate Makefile
    (_, _, _, ph) <- createProcess (proc "./smake/smake" ["cherry"]) {cwd = Just path}
    rc <- waitForProcess ph

    let build_dir = path </> "build" </> "cherry"

    -- copy and compile assembly files
    forM sources $ \(fn, contents) -> do
        let src = build_dir </> fn
        BS.writeFile src contents
        let obj = src -<.> "o"
        if takeExtension fn == ".s" then do
            (_, _, _, ph) <- createProcess (proc "arm-none-eabi-as" [src, "-o", obj])
            rc <- waitForProcess ph
            putStrLn ("Assembly result: " ++ show rc)
        else
            return ()

    -- modify Makefile
    let objs = foldl (\a (n, _) -> a ++ " " ++ (n -<.> "o")) "" sources
    let cfiles = map fst $ filter (\(n, _) -> takeExtension n == ".c") sources
    makefile <- TI.readFile (build_dir </> "Makefile")
    let makefile_lines = map (\l -> if "OBJS :=" `T.isPrefixOf` l then l <> T.pack objs else l) (T.lines makefile)
    let c_rules = map (\f -> (f -<.> "o") ++ ": " ++ f ++ "\n\t$(CC) $(CFLAGS) -c $< -o $@") cfiles
    let makefile_new = makefile_lines ++ (map T.pack c_rules)
    TI.writeFile (build_dir </> "Makefile") $ T.unlines makefile_new

    -- run make
    (_, _, _, ph) <- createProcess (proc "make" []) {cwd = Just build_dir}
    rc <- waitForProcess ph

    -- read results
    flash <- BS.readFile (build_dir </> "app.flash")
    hex <- BS.readFile (build_dir </> "app.hex")

    -- get addresses from ELF file
    elf_symbols <- lines <$> readProcess "arm-none-eabi-nm" ["--print-size", build_dir </> "app.nobl.elf"] ""
    let lookup_symbol sym = read . ("0x" ++) . head . words . head $ filter (\e -> let l = words e in last l == sym) elf_symbols 
    let emulator_main_addr = lookup_symbol "emulator_main"
    let emulator_cdl_start_addr = lookup_symbol "emulator_cdl_start"
    let emulator_exit_addr = lookup_symbol "emulator_exit"

    return $ BuildResults { ident, flash, hex, emulator_main_addr, emulator_cdl_start_addr, emulator_exit_addr, symbols }