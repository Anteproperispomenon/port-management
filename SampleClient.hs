{-# LANGUAGE OverloadedStrings #-}

-- A sample client for testing the code.

module Main where

import qualified Data.ByteString.Lazy as BL

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put


import Data.Connection

import qualified System.IO.Streams     as SR
import qualified System.IO.Streams.TCP as TCP

import System.IO.Streams.TCP (TCPConnection)

import System.IO.Streams.Binary

import System.IO
import System.Environment

import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.IO       as T
import qualified Data.Text.Lazy.Encoding as T

import Types.SampleMessage

import Network.Socket hiding (close)

import Control.Concurrent.STM
import Control.Concurrent.Async

import Control.Exception

import Data.Maybe

main :: IO ()
main = do
    { hSetEncoding stdin  utf8
    ; hSetEncoding stdout utf8
    ; hSetEncoding stderr utf8
    ; [srv,pn] <- getArgs
    ; let pnum = (read pn) :: PortNumber
    ; con1 <- TCP.connect srv pnum
    ; npn  <- fromIntegral <$> fromJust <$> (getFromStream getWord16le $ source con1)
    ; close con1
    ; conn <- TCP.connect srv npn
    ; tqin <- newTQueueIO
    ; tqtx <- newTQueueIO
    -- To try to ensure that the receiver thread is the one that calls 'close'.
    ; withAsync ((queuerThread tqin conn) `finally` (putStrLn "Closing connection" >> close conn)) $ \_ -> do
        { withAsync ((writerThread tqtx) `finally` (putStrLn "Exiting writerThread")) $ \_ -> do
            { (mainThread conn tqin tqtx) `onException` ((send conn $ encode ClientExit))
            }
        }
    }
-- asdfzxcv

-- Receive messages from the server, and write them to a queue.
queuerThread :: TQueue ServerMessage -> TCPConnection -> IO ()
queuerThread tq conn = do
    { x <- decodeFromStream $ source conn
    ; case x of
        { (Just x) -> atomically $ writeTQueue tq x
        ; Nothing  -> return ()
        }
    ; print x
    ; queuerThread tq conn
    }
-- asdfzxcv

-- Take input from the user, and write it to a queue.
writerThread :: TQueue ClientMessage -> IO ()
writerThread tq = do
    { txt <- T.getLine
    ; let txt' = T.toLower txt
    ; if ("exit" `T.isPrefixOf` txt')
        then (atomically (writeTQueue tq ClientExit))
        else do
            { atomically $ writeTQueue tq $ ClientText txt
            ; writerThread tq
            }
    }
-- asdfzxcv

-- Examine the queues of data from the server and the client, and
-- process/send messages from them respectively.
mainThread :: TCPConnection -> TQueue ServerMessage -> TQueue ClientMessage -> IO ()
mainThread conn tqin tqout = do
 -- { rslt <- race (atomically $ readTQueue tqin) (atomically $ readTQueue tqout)
    { rslt <- atomically $ raceSTM (readTQueue tqin) (readTQueue tqout)
    ; case rslt of
        { (Left (ServerText txt)) -> do
            { T.putStrLn txt
            ; mainThread conn tqin tqout
            }
        ; (Left ServerExit) -> do
            { T.putStrLn $ "Server exiting."
            }
        ; (Right cmsg@(ClientText txt)) -> do
            { send conn $ encode cmsg
            ; mainThread conn tqin tqout
            }
        ; (Right cmsg@(ClientExit)) -> do
            { send conn $ encode cmsg
            ; T.putStrLn "Exiting..."
            }
        }
    }
-- asdfzxcv


-- Not really race, but something like it.
-- Really just get the first piece of data
-- from one of two queues, with a left bias.
raceSTM :: STM a -> STM b -> STM (Either a b)
raceSTM sta stb = (Left <$> sta) `orElse` (Right <$> stb)


