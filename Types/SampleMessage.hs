--

module Types.SampleMessage
    ( ClientMessage(..)
    , ServerMessage(..)
    , putClientMessage
    , getClientMessage
    , putServerMessage
    , getServerMessage
    ) where
-- asdfzxcv

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.Encoding as T

import qualified Data.ByteString.Lazy as BL

data ClientMessage
   = ClientText T.Text
   | ClientExit
   deriving (Show,Eq)
-- asdfzxcv



putClientMessage :: ClientMessage -> Put
putClientMessage (ClientText txt) = do
    { putWord8 0x10
    ; let bs = T.encodeUtf8 txt
    ; putWord32le (fromIntegral $ BL.length bs)
    ; putLazyByteString bs
    }
putClientMessage (ClientExit) = putWord8 0x20
-- asdfzxcv

getClientMessage :: Get ClientMessage
getClientMessage = do
    { cd <- getWord8
    ; case cd of
        { 0x10 -> do
            { len <- getWord32le
            ; bs  <-getLazyByteString (fromIntegral len)
            ; return $ ClientText $ T.decodeUtf8 bs
            }
        ; 0x20 -> return ClientExit
        ; _    -> fail "Error: No parse for client message."
        }
    }
-- asdfzxcv

instance Binary ClientMessage where
    put = putClientMessage
    get = getClientMessage
-- asdfzxcv

data ServerMessage
   = ServerText T.Text
   | ServerExit
   deriving (Show,Eq)
-- asdfzxcv

putServerMessage :: ServerMessage -> Put
putServerMessage (ServerText txt) = do
    { putWord8 0x30
    ; let bs = T.encodeUtf8 txt
    ; putWord32le (fromIntegral $ BL.length bs)
    ; putLazyByteString bs
    }
putServerMessage (ServerExit) = putWord8 0x40
-- asdfzxcv

getServerMessage :: Get ServerMessage
getServerMessage = do
    { cd <- getWord8
    ; case cd of
        { 0x30 -> do
            { len <- getWord32le
            ; bs  <-getLazyByteString (fromIntegral len)
            ; return $ ServerText $ T.decodeUtf8 bs
            }
        ; 0x40 -> return ServerExit
        ; _    -> fail "Error: No parse for server message."
        }
    }
-- asdfzxcv

instance Binary ServerMessage where
    put = putServerMessage
    get = getServerMessage
-- asdfzxcv
