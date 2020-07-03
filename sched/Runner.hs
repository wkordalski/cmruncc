{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module Runner (handleRunnerClient) where

import Control.Concurrent
import Network.Socket

import Common (RunQueue, ResponseQueue, BuildResultsStorage, takeResultFromStorage, handleWorker)
import CMRunCC.Messages (BuildResults (..), RunRequest (..), RunResults (..), PublicAPIResponse (..))

handleRunnerClient :: RunQueue -> ResponseQueue -> BuildResultsStorage -> Socket -> IO ()
handleRunnerClient run_queue response_queue storage sock =
    handleWorker
        (\r -> let RunRequest {ident} = r in ident)
        (\r -> let RunResults {ident} = r in ident)
        run_queue
        (response_queue, storage)
        on_response
        sock
    
on_response (response_queue, storage) request = do
    let RunResults { ident, symbols } = request
    BuildResults {
            flash,
            emulator_main_addr, emulator_cdl_start_addr, emulator_exit_addr
        } <- takeResultFromStorage storage ident
    let response = PublicAPIResponse {
            ident, flash,
            emulator_main_addr, emulator_cdl_start_addr, emulator_exit_addr,
            mem_dump=symbols
        }
    writeChan response_queue response
