{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module CMRunCC.Messages (
    readMessage, writeMessage,
    RunRequest (..), PublicAPIRequest (..), PublicAPIResponse (..),
    BuildResults (..)) where

import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.MessagePack.Object as MP
import Data.Bits
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Word

-- Vector construction and lookup

(~~>) :: MP.MessagePack b => String -> b -> (MP.Object, MP.Object)
(~~>) k v = (MP.ObjectStr (T.pack k), MP.toObject v)

(!~>) :: MP.MessagePack a => V.Vector (MP.Object, MP.Object) -> String -> Maybe a
(!~>) v k =
    let filtered = V.filter (\e ->
            case e of
            (MP.ObjectStr t, _) -> (T.unpack t == k)
            _ -> False
            ) v
    in if V.null filtered then Nothing else MP.fromObject $ snd $ V.head filtered

expectMap :: MP.Object -> Maybe (V.Vector (MP.Object, MP.Object))
expectMap (MP.ObjectMap v) = return v
expectMap _ = Nothing

makeMap :: [(MP.Object, MP.Object)] -> MP.Object
makeMap = MP.ObjectMap . V.fromList

-- Message (de-)serialization

u32ToByteString :: Int -> ByteString
u32ToByteString v = B.pack $ map fromIntegral [
        v .&. 0xFF,
        (v .&. 0xFF00) `shiftR` 8,
        (v .&. 0xFF0000) `shiftR` 16,
        (v .&. 0xFF000000) `shiftR` 24
    ]

byteStringToU32 :: ByteString -> Int
byteStringToU32 s = B.foldr (\b a -> a `shiftL` 8 .|. (fromIntegral b)) 0 s

readExact :: Socket -> Int -> IO (Maybe ByteString)
readExact s n = helper n B.empty
    where
        helper 0 acc = return $ Just acc
        helper rest acc = do
            d <- recv s rest
            if B.null d then
                return Nothing
            else
                helper (rest - B.length d) (B.append acc d)


readMessage :: Socket -> IO (Maybe ByteString)
readMessage s = do
    header <- readExact s 4
    case header of
        Just h -> readExact s (byteStringToU32 h)
        Nothing -> return Nothing

writeMessage :: Socket -> ByteString -> IO ()
writeMessage s m = do
    let n = B.length m
    sendAll s (u32ToByteString n)
    sendAll s m

-- Message types

data PublicAPIRequest = PublicAPIRequest {
    ident :: String,
    build_spec :: ByteString,
    sources :: [(String, ByteString)],
    symbols :: [(String, Int)]
} deriving Show

instance MP.MessagePack PublicAPIRequest where
    toObject (PublicAPIRequest { ident, build_spec, sources, symbols }) = makeMap [
            "ident" ~~> ident,
            "build_spec" ~~> build_spec,
            "sources" ~~> sources,
            "symbols" ~~> symbols
        ]
    fromObject o = do
        v <- expectMap o
        ident <- v !~> "ident"
        build_spec <- v !~> "build_spec"
        sources <- v !~> "sources"
        symbols <- v !~> "symbols"
        return $ PublicAPIRequest { ident, build_spec, sources, symbols }

data PublicAPIResponse = PublicAPIResponse {
    ident :: String,
    flash :: ByteString,
    emulator_main_addr :: Int,
    emulator_cdl_start_addr :: Int,
    emulator_exit_addr :: Int,
    mem_dump :: [(String, Int, ByteString)]
}

instance MP.MessagePack PublicAPIResponse where
    toObject (PublicAPIResponse { ident, flash, emulator_main_addr, emulator_cdl_start_addr, emulator_exit_addr, mem_dump}) =
        makeMap [
                "ident" ~~> ident,
                "flash" ~~> flash,
                "emulator_main_addr" ~~> emulator_main_addr,
                "emulator_cdl_start_addr" ~~> emulator_cdl_start_addr,
                "emulator_exit_addr" ~~> emulator_exit_addr,
                "mem_dump" ~~> mem_dump
            ]

    fromObject o = do
        v <- expectMap o
        ident <- v !~> "ident"
        flash <- v !~> "flash"
        emulator_main_addr <- v !~> "emulator_main_addr"
        emulator_cdl_start_addr <- v !~> "emulator_cdl_start_addr"
        emulator_exit_addr <- v !~> "emulator_exit_addr"
        mem_dump <- v !~> "mem_dump"
        return $ PublicAPIResponse {
                ident, flash,
                emulator_main_addr, emulator_cdl_start_addr, emulator_exit_addr,
                mem_dump
            }

data BuildResults = BuildResults {
    ident :: String,
    flash :: ByteString,
    hex :: ByteString,
    emulator_main_addr :: Int,
    emulator_cdl_start_addr :: Int,
    emulator_exit_addr :: Int,
    symbols :: [(String, Int)]
} deriving Show

instance MP.MessagePack BuildResults where
    toObject (BuildResults { ident, flash, hex, emulator_main_addr, emulator_cdl_start_addr, emulator_exit_addr, symbols }) = makeMap [
            "ident" ~~> ident,
            "flash" ~~> flash,
            "hex" ~~> hex,
            "emulator_main_addr" ~~> emulator_main_addr,
            "emulator_cdl_start_addr" ~~> emulator_cdl_start_addr,
            "emulator_exit_addr" ~~> emulator_exit_addr,
            "symbols" ~~> symbols
        ]

    fromObject o = do
        v <- expectMap o
        ident <- v !~> "ident"
        flash <- v !~> "flash" :: Maybe ByteString
        hex <- v !~> "hex" :: Maybe ByteString
        emulator_main_addr <- v !~> "emulator_main_addr"
        emulator_cdl_start_addr <- v !~> "emulator_cdl_start_addr"
        emulator_exit_addr <- v !~> "emulator_exit_addr"
        symbols <- v !~> "symbols"
        return $ BuildResults { ident, flash, hex, emulator_main_addr, emulator_cdl_start_addr, emulator_exit_addr, symbols }


data RunRequest = RunRequest {
    hex :: ByteString,
    ident :: ByteString
} deriving Show

instance MP.MessagePack RunRequest where
    toObject (RunRequest { hex = hex, ident = ident }) = makeMap [
            "hex" ~~> MP.toObject hex,
            "ident" ~~> MP.toObject ident
        ]
    fromObject o = do
        v <- expectMap o
        hex <- v !~> "hex"
        ident <- v !~> "ident"
        return $ RunRequest { hex = hex, ident = ident }

