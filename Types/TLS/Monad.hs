{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE UndecidableInstances #-} -- ugh...
-- {-# LANGUAGE TypeFamilies #-}

{-|
Module      : Types.TCP.Monad
Description : A monad for use over TCP, together with the server.
Copyright   : David Wilson, 2021
License     : BSD-3
Stability   : Unknown
Portability : Unknown

This is a monad that simplifies sending and receiving
data over a TCP connection. Note that it requires that
you provide a Binary getter for your data type in order
to use it.

This is just a copy of Types.TCP.Monad, but specialised
to a different type.

-}

-- If you prefer to use a monad that conceals the values of
-- the connection, this is (probably?) for you.

module Types.TLS.Monad
    ( -- * Types and Running
      TLST
    , TLSR
    , TLS
    , runTLS
    , runTLST
    , runTLS'
    -- * Actually sending/receiving data
    , recv
    , recvMaybe
    , recvSTM
    , recvOrElse
    , recvOrElseEither
    , sendLazy
    , sendStrict
    , sendBinary 
    , sendPut
    -- * Other functions
    , getConnection
    , getResource
    , withTLST
--  , atomicallyTLS
    , atomicallyTLST
    ) where
-- asdfzxcv

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Control.Monad.Trans.Reader

-- import Control.Exception

import Control.Monad.Catch

import Control.Exception (MaskingState(..),getMaskingState,interruptible,allowInterrupt)

import Control.Concurrent.STM


import Control.Concurrent.Async

import qualified System.IO.Streams     as SR
import qualified System.IO.Streams.TCP as TCP

import System.IO.Streams.TCP (TCPConnection)
import System.IO.Streams.TLS (TLSConnection)

import Network.TLS (Context)

import System.IO.Streams.Binary

-- import Control.Monad.Trans
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Network.Socket (Socket,SockAddr)

import Data.Connection

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL

import Types.Connection.Monad (ConnectionT)

import Types.Connection.Monad as I

-- import Control.Monad.Trans.Control
-- import Control.Monad.Base

-- Using newtype so that it can't be (mis-)used with the ReaderT interface by users.
-- type TCPT i m a =        ReaderT (TQueue x, TCPConnection, (x -> i)) m a
-- data TCPT i m a = forall x. TCPT ( ReaderT (TQueue x, TCPConnection, (x -> i)) m a )

-- | This is the type of the TLS Transformer. Under the hood, it is a
-- a wrapped ReaderT that has a queue (for receiving data) and a TCPConnection.
--
-- Note that this type is explicitly designed for use in this package; it does not
-- close the connection automatically, and you have to open the connection yourself.
--
-- You'll also probably want to make sure you send an 'exit' message to the server/client
-- in the event of an exception, so you'll probably want to use 'bracket'/'bracketOnException'/etc...
-- 

-- newtype TCPT i r m a = TCPT ( ReaderT (PolyQueue i, TCPConnection, r) m a ) deriving (Functor,Applicative,Monad,MonadTrans,MonadIO,MonadCatch,MonadThrow,MonadMask)
type TLST i r m a = ConnectionT i r (Context, SockAddr) m a

-- | A TCP transformer directly based on IO.
type TLSR i r a = TLST i  r IO a

-- | A TCP transformer directly based on IO, without a shared resource.
type TLS  i   a = TLST i () IO a


-- A queue that has an unknowable inner type, but a knowable
-- extraction type.
-- data PolyQueue a = forall x. PolyQueue (TQueue x) (x -> a)

-- So that we can change the output type of a queue, without
-- having to map over all the values of the queue. (Which
-- wouldn't be possible, anyway).
-- instance Functor PolyQueue where
--     fmap f (PolyQueue tq g) = PolyQueue tq (f . g)
-- asdfzxcv

-- Gives you two interfaces to the queue:
-- Read from the first one,
-- write to the second one.
-- newPolyQueue :: STM (PolyQueue a, TQueue a)
-- newPolyQueue = do
--     { tq <- newTQueue
--     ; return (PolyQueue tq id, tq)
--     }
-- asdfzxcv

-- readPolyQueue :: PolyQueue a -> STM a
-- readPolyQueue (PolyQueue tq f) = f <$> readTQueue tq

-- tryReadPolyQueue :: PolyQueue a -> STM (Maybe a)
-- tryReadPolyQueue (PolyQueue tq f) = (fmap f) <$> tryReadTQueue tq

-- | Like 'withReaderT', but only on the queue's output.
--
-- This can be useful if you have a "layered" message style,
-- e.g.
--
-- @
-- import qualified Data.Text.Lazy          as T
-- import qualified Data.Text.Lazy.Encoding as T
--
-- import qualified Data.ByteString.Lazy as BL
--
-- import Data.Binary
-- import Data.Binary.Get
--
-- import Data.Word
--
-- import Types.TCP.Monad
--
-- data BasicMessage = BasicMessage BL.ByteString deriving (Show,Eq)
--
-- getBasicMessage :: Get BasicMessage
-- getBasicMessage = do { len <- getInt32le ; bs <- getLazyByteString (fromIntegral len) ; return $ BasicMessage bs}
--
-- data SignedMessage = SignedMessage T.Text BL.ByteString deriving (Show,Eq)
-- 
-- getSignedMessage :: Get SignedMessage
-- getSignedMessage = do
--    { len1 <- getInt32le
--    ; nm   <- T.decodeUtf8 <$\> getLazyByteString (fromIntegral len1)
--    ; len2 <- getInt32le
--    ; bs   <- getLazyByteString (fromIntegral len2)
--    ; return $ SignedMessage txt bs
--    }
--
-- data StatusMessage = Okay  Word32 Word32
--                    | Error Word32 Word32
--                    deriving (Show,Eq)
-- 
-- getStatusMessage :: Get StatusMessage
-- getStatusMessage = do
--    { st <- getWord8
--    ; w1 <- getWord32le
--    ; w2 <- getWord32le
--    ; return $ if (st == 0x00) then (Okay w1 w2) else (Error w1 w2)
--    }
--
-- basicToSigned :: BasicMessage -> SignedMessage
-- basicToSigned (BasicMessage bs) = runGet getSignedMessage bs
--
-- signedToStatus :: SignedMessage -> (Text,StatusMessage)
-- signedToStatus (SignedMessage nm bs) = (nm,runGet getStatusMessage bs)
--
-- basicToStatus :: BasicMessage -> StatusMessage
-- basicToStatus (BasicMessage bs) = runGet getStatusMessage
-- 
-- myConnection :: TCP BasicMessage ()
-- myConnection = do
--    { withTCPT basicToStatus $ do
--        { msg <- recv
--        ; liftIO $ print msg
--        }
--    ; withTCPT basicToSigned $ do
--        { withTCPT signedToStatus $ do
--            { (nm, msg) <- recv
--            ; liftIO $ putStrLn $ "Status: " ++ (show msg) ++ ", signed by " ++ (T.unpack nm) ++ "." 
--            }
--        }
--    }
-- @
-- 
-- Reason for the "backwards" application:
-- 
-- Say you have a (TCPT b m o) block, but you want to run it in
-- a (TCPT a m o) block. This means you want to "simulate" a 
-- (TCPT b m o) inside a (TCPT a m o). To do this, you have to
-- convert your queue of as into a queue of bs. Thus, you place
-- an "adapter" of type (a -> b) on the output of the queue, and 
-- now you have a queue of type b. Now you can run the (TCPT b m o)
-- computation inside your (TCPT a m o) computation.
withTLST :: (MonadIO m) => (a -> b) -> TLST b r m c -> TLST a r m c
withTLST = I.withConnectionT


-- | Run a TCP connection monad. It spawns a new thread
-- that receives data from the client, and then writes it/them
-- to a queue that the computation has access to. 
-- This is probably the function you're looking for, if you're trying to
-- run it yourself.
--
-- To run this with "Server.TCP.Public", use 
--
-- > runAsyncServerWithResource acquire release pnum comLimit lisLimit (runTCP action getter)
--
-- Where
-- 
-- * acquire  : Set up / acquire the resource to be shared between the threads (often some type of STM variable).
-- * release  : Clean up / deallocate the shared resource. 
-- * pnum     : The initial port number on which to listen for new connections.
-- * comLimit : The maximum number of simultaneous connections for each communication socket.
-- * lisLimit : The maximum number of simultaneous connections on the listen socket.
-- * action   : The actual TLST action to run for each connection.
-- * getter   : The Binary Getter to parse values from the client.
--
-- At the moment, this is probably safer than 'runTLST' since this version
-- makes use of 'withAsync' directly rather than a modified version based 
-- on 'asyncWithUnmask'.
runTLS :: (TLST i r IO a) -> (Get i) -> r -> TLSConnection -> IO a
runTLS = I.runConnectionM -- (TLST rdr) gtr rsrc conn = do
--  { (pq,tq) <- atomically newPolyQueue
--  ; withAsync (receiver conn tq gtr) (\_ -> runReaderT rdr (pq,conn,rsrc))
--  }
-- asdfxzcv

-- | A simplified version of runTCP, when you don't have a shared resource.
-- To run with "Server.TCP.Public", use
--
-- > runAsyncServerSimple pnum comLimit lisLimit (runTCP' action getter)
-- 
-- ... with the same values as for 'runTCP'.
runTLS' :: (TLS i a) -> (Get i) -> TLSConnection -> IO a
runTLS' = I.runConnection' -- rdr gtr conn = runTCP rdr gtr () conn

-- | Run a TLST monad that is not directly over IO.
-- This makes use of 'asyncWithUnmask', since the
-- spawned thread only needs to be of type IO.
runTLST :: (MonadIO m, MonadMask m) => (TLST i r m a) -> (Get i) -> r -> TLSConnection -> m a
runTLST = I.runConnectionT -- (TLST rdr) gtr rsrc conn = do
--  { (pq,tq) <- liftIO $ atomically newPolyQueue
--  ; withAsyncT (receiver conn tq gtr) (\_ -> runReaderT rdr (pq,conn,rsrc))
--  }
-- asdfxzcv

-- DO NOT EXPORT.
-- receiver :: TCPConnection -> TQueue i -> Get i -> IO ()
-- receiver conn tq gtr = do
--     { msg <- getFromStream gtr (source conn)
--     ; maybe (return ()) (\x -> atomically $ writeTQueue tq x) msg
--     ; receiver conn tq gtr
--     }
-- asdfzxcv


-- DO NOT EXPORT.
-- receiverBinary :: (Binary i) => TCPConnection -> TQueue i -> IO ()
-- receiverBinary conn tq = do
--     { msg <- decodeFromStream (source conn)
--     ; maybe (return ()) (\x -> atomically $ writeTQueue tq x) msg
--     ; receiverBinary conn tq
--     }
-- asdfzxcv

-- Old versions of the code that
-- use specialised signatures.

-- Removed so that one can use both Types.TLS.Monad
-- and Types.TCP.Monad in the same module/program
-- without having to import them qualified.

{-

-- | Receive an i from the connection. This function
-- will block if there is no data to be received in
-- the queue.
recv :: (MonadIO m) => TLST i r m i
recv = I.recv -- = TLST $ do
--  { (tq,_,_) <- ask
--  ; liftIO $ atomically $ readPolyQueue tq
--  }
-- asdfzxcv

-- | Receive an STM transaction that
-- will try to retrieve the next value.
-- 
-- Awkward to use, e.g.
--
-- @
-- do { trnsct <- recvSTM
--    ; msg <- atomically (trnsct `orElse` altTrnsct)
--    ; return msg
--    }
-- @
recvSTM :: (Monad m) => TLST i r m (STM i)
recvSTM = I.recvSTM -- = TLST $ do
--  { (tq,_,_) <- ask
--  ; return (readPolyQueue tq)
--  }
-- asdfzxcv

-- | Slightly less awkward version of 'recvSTM', but less powerful.
recvOrElse :: (MonadIO m) => STM i -> TLST i r m i
recvOrElse = I.recvOrElse -- actn = TLST $ do
--  { (tq,_,_) <- ask
--  ; liftIO $ atomically ((readPolyQueue tq) `orElse` actn)
--  }
-- sadfzxcv

-- | Slightly more useful version of recvOrElse.
recvOrElseEither :: (MonadIO m) => STM j -> TLST i r m (Either j i)
recvOrElseEither = I.recvOrElseEither -- actn = TLST $ do
--  { (tq,_,_) <- ask
--  ; liftIO $ atomically ((Right <$> readPolyQueue tq) `orElse` (Left <$> actn))
--  }
-- asdfzxcv

-- | Receive an i from the connection, if there
-- is one available. This will never block, since
-- it will return Nothing if there isn't anything
-- in the receiver queue.
recvMaybe :: (MonadIO m) => TLST i r m (Maybe i)
recvMaybe = I.recvMaybe -- = TLST $ do
--  { (tq,_,_) <- ask
--  ; liftIO $ atomically $ tryReadPolyQueue tq
--  }
-- asdfzxcv

-- | Send a lazy bytestring to the connected client.
sendLazy :: (MonadIO m) => BL.ByteString -> TLST i r m ()
sendLazy = I.sendLazy -- bs = TLST $ do
--  { (_,conn,_) <- ask
--  ; liftIO $ send conn bs
--  }
-- asdfzxcv

-- | Send a strict bytestring to the connected client.
sendStrict :: (MonadIO m) => BS.ByteString -> TLST i r m ()
sendStrict = I.sendStrict -- bs = TLST $ do
--  { (_,conn,_) <- ask
--  ; liftIO $ send conn (BL.fromStrict bs)
--  }
-- asdfzxcv

-- | Encode a binary value, and then send it to the client.
sendBinary :: (MonadIO m, Binary b) => b -> TLST i r m ()
sendBinary = I.sendBinary -- x = TLST $ do
--  { (_,conn,_) <- ask
--  ; liftIO $ send conn (encode x)
--  }
-- asdfxzcv

-- | Encode a value with a binary putter, and then send it
-- to the client. Most useful if you just write a version
-- specialised to your getter, e.g.
--
-- > sendMessage = sendPut putMessage
--
-- ...or if you're mapping over several values, e.g.
--
-- > mapM_ (sendPut putter) [x,y,z]
--
sendPut :: (MonadIO m) => (a -> Put) -> a -> TLST i r m ()
sendPut = I.sendPut -- pf x = TLST $ do
--  { (_,conn,_) <- ask
--  ; liftIO $ send conn (runPut $ pf x)
--  }
-- asdfzxcv


-- | Only use this if you want send data from one client
-- to another.
getConnection :: (MonadIO m) => TLST i r m TLSConnection
getConnection = I.getConnection -- = TLST $ do { (_,conn,_) <- ask ; return conn }
-- asdfzxcv

-- | Use this to access the shared resource accessible to
-- multiple connections.
getResource  :: (MonadIO m) => TLST i r m r
getResource = I.getResource -- = TLST $ do { (_,_,rsrc) <- ask ; return rsrc }

-- A shorthand to ease uses of STM.
-- atomicallyTCP :: (STM a) -> TLST i r IO a
-- atomicallyTCP action = TLST $ atomically action

-}

-- | A shorthand to ease uses of STM.
atomicallyTLST :: (MonadIO m) => (STM a) -> TLST i r m a
atomicallyTLST = I.atomicallyConn -- action = TLST $ liftIO (atomically action)


---------------------------------
-- Allowing Stuff like runTLST --
---------------------------------

-- According to "Control.Concurrent.Async"...
--
-- > withAsync action inner = mask $ \restore -> do
-- >  a <- async (restore action)
-- >  restore (inner a) `finally` uninterruptibleCancel a
-- 
-- No idea whether this has the desired behaviour. I suspect it doesn't.
--
-- withAsyncT :: (MonadIO m, MonadMask m) => IO a -> (Async a -> m b) -> m b
-- withAsyncT action inner = mask $ \restore -> do
--     { a <- liftIO $ asyncWithUnmask (\rstr -> rstr action)
--     ; restore (inner a) `finally` (liftIO $ uninterruptibleCancel a)
--     }
-- asdfzxcv


-- printMaskingState :: (MonadIO m) => m ()
-- printMaskingState = do
--     { msk <- liftIO getMaskingState
--     ; liftIO $ print msk
--     }
-- asdfzxcv


