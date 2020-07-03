{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module Builder (handleBuilderClient) where

import Control.Concurrent
import Network.Socket

import CMRunCC.Messages (PublicAPIRequest (..), BuildResults (..), RunRequest (..))
import Common (BuildQueue, RunQueue, BuildResultsStorage, insertResultIntoStorage, handleWorker)

handleBuilderClient :: BuildQueue -> RunQueue -> BuildResultsStorage -> Socket -> IO ()
handleBuilderClient build_queue run_queue storage sock =
    handleWorker
        (\r -> let PublicAPIRequest {ident} = r in ident)
        (\r -> let BuildResults {ident} = r in ident)
        build_queue
        (run_queue, storage)
        on_response
        sock

on_response (run_queue, storage) request = do
    let BuildResults { ident, symbols, hex } = request
    insertResultIntoStorage storage request
    writeChan run_queue $ RunRequest { hex, ident, symbols }
