
module ChatMessage
    ( ClientMessage(..)
    , getClientMessage
    , putClientMessage
    , ServerMessage(..)
    , getServerMessage
    , putServerMessage
    ) where
-- asdfxzcv


import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL

import Data.Word

import Data.Time

import Control.Monad

data ClientMessage
   = ClientSetup T.Text          -- ^ Add yourself to the group. Necessary before sending messages.
   | ClientBrowse Int Int        -- ^ Browse the names/ids of 'n' clients, starting from a certain number.
   | ClientPrivate Word64 T.Text -- ^ Send a private message to a single user.
   | ClientBroadcast      T.Text -- ^ Send a message to all users.
   | ClientExit                  -- ^ Exit the connection.
   deriving (Show,Eq)
-- asdfxzcv

instance Binary ClientMessage where
    get = getClientMessage
    put = putClientMessage
-- asdfzxcv


getClientMessage :: Get ClientMessage
getClientMessage = do
    { x <- getWord8
    ; case x of
        { 0xC0 -> return ClientExit
        ; 0xC1 -> do
            { nm <- getSomeText
            ; return $ ClientSetup nm
            }
        ; 0xC2 -> do
            { txt <- getSomeText
            ; return $ ClientBroadcast txt
            }
        ; 0xC3 -> do
            { uid <- getWord64le
            ; txt <- getSomeText
            ; return $ ClientPrivate uid txt
            }
        ; 0xC4 -> do
            { strt <- fromIntegral <$> getInt32le
            ; leng <- fromIntegral <$> getInt32le
            ; return $ ClientBrowse strt leng
            }
        }
    }
-- asdfzxcv

putClientMessage :: ClientMessage -> Put
putClientMessage (ClientSetup nm) = do
    { putWord8 0xC1
    ; putSomeText nm
    }
putClientMessage (ClientBroadcast txt) = do
    { putWord8 0xC2
    ; putSomeText txt
    }
putClientMessage (ClientPrivate uid txt) = do
    { putWord8 0xC3
    ; putWord64le uid
    ; putSomeText txt
    }
putClientMessage (ClientBrowse strt leng) = do
    { putWord8 0xC4
    ; putWord32le $ fromIntegral strt
    ; putWord32le $ fromIntegral leng
    }
putClientMessage (ClientExit) = putWord8 0xC0

data ServerMessage
   = ServerChat     T.Text Word64 T.Text UTCTime -- ^ Sender, User ID, Message, Timestamp
   | ServerPrivate  T.Text Word64 T.Text UTCTime -- ^ Sender, User ID, Message, Timestamp
   | ServerAlert    T.Text UTCTime               -- ^ A general alert from the server that is sent to all users.
   | ServerSetup    Word64                       -- ^ Succesfully setup user; here's your id.
   | ServerUserList [(Word64,T.Text)]            -- ^ Result from using browse.
   | ServerError    T.Text                       -- ^ Any kind of error.
   | ServerExit                                  -- ^ Server has exited.
   deriving (Show,Eq)
-- asdfxzcv

instance Binary ServerMessage where
    get = getServerMessage
    put = putServerMessage
-- asdfzxcv

getServerMessage :: Get ServerMessage
getServerMessage = do
    { x <- getWord8
    ; case x of
        { 0x50 -> return ServerExit
        ; 0x51 -> do
            { uid <- getWord64le
            ; return $ ServerSetup uid
            }
        ; 0x52 -> do
            { unm <- getSomeText
            ; uid <- getWord64le
            ; txt <- getSomeText
            ; tim <- getUTCTime
            ; return $ ServerChat unm uid txt tim
            }
        ; 0x53 -> do
            { unm <- getSomeText
            ; uid <- getWord64le
            ; txt <- getSomeText
            ; tim <- getUTCTime
            ; return $ ServerPrivate unm uid txt tim
            }
        ; 0x54 -> do 
            { txt <- getSomeText
            ; tim <- getUTCTime
            ; return $ ServerAlert txt tim
            }
        ; 0x55 -> do
            { num  <- fromIntegral <$> getInt32le
            ; usrs <- replicateM num getUser
            ; return $ ServerUserList usrs
            }
        ; 0x5F -> do
            { txt <- getSomeText
            ; return $ ServerError txt
            }
        }
    }
-- asdfzxcv

putServerMessage :: ServerMessage -> Put
putServerMessage (ServerSetup uid) = do
    { putWord8 0x51
    ; putWord64le uid
    }
putServerMessage (ServerChat unm uid txt tim) = do
    { putWord8 0x52
    ; putSomeText unm
    ; putWord64le uid
    ; putSomeText txt
    ; putUTCTime tim
    }
putServerMessage (ServerPrivate unm uid txt tim) = do
    { putWord8 0x53
    ; putSomeText unm
    ; putWord64le uid
    ; putSomeText txt
    ; putUTCTime tim
    }
putServerMessage (ServerAlert txt tim) = do
    { putWord8 0x54
    ; putSomeText txt
    ; putUTCTime tim
    }
putServerMessage (ServerUserList usrs) = do
    { putWord8 0x55
    ; let len = length usrs
    ; putInt32le $ fromIntegral len
    ; mapM_ putUser usrs
    }
putServerMessage (ServerError txt) = do
    { putWord8 0x5F
    ; putSomeText txt
    }
putServerMessage ServerExit = putWord8 0x50
-- asdfzxcv


-------------
-- Helpers --
-------------

-- Use (replicateM n parser) to repeat the same parser n times.

getUser :: Get (Word64,T.Text)
getUser = do
    { uid <- getWord64le
    ; txt <- getSomeText
    ; return (uid,txt)
    }
-- asdfzxcv

putUser :: (Word64,T.Text) -> Put
putUser (uid,nm) = do
    { putWord64le uid
    ; putSomeText nm
    }
-- adsfzxcv

getSomeText :: Get T.Text
getSomeText = do
    { len <- getInt32le
    ; T.decodeUtf8 <$> getByteString (fromIntegral len)
    }
-- asdfxzcv

putSomeText :: T.Text -> Put
putSomeText txt = do
    { let bs = T.encodeUtf8 txt
    ; putInt32le $ fromIntegral $ BS.length bs
    ; putByteString bs
    }
--- asdfzxcv

getSomeBytes :: Get BS.ByteString
getSomeBytes = do
    { len <- getInt32le
    ; getByteString (fromIntegral len)
    }
-- asdfxzcv

putSomeBytes :: BS.ByteString -> Put
putSomeBytes bs = do
    { putInt32le $ fromIntegral $ BS.length bs
    ; putByteString bs
    }
-- asdfxzcv

-- It works!
-- do { tim <- getCurrentTime ; return $ (tim, runGet getUTCTime $ runPut (putUTCTime tim)) }
getUTCTime :: Get UTCTime
getUTCTime = do
    { day <- ModifiedJulianDay     <$> get
    ; tim <- picosecondsToDiffTime <$> get
    ; return $ UTCTime day tim
    }
-- asdfzxcv

putUTCTime :: UTCTime -> Put
putUTCTime (UTCTime day tim) = do
    { put $ toModifiedJulianDay   day
    ; put $ diffTimeToPicoseconds tim
    }
-- asdfzxcv


