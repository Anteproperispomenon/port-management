{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe

import Control.Monad
import Control.Applicative

import qualified Data.Text as T

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put


import System.IO
import System.Environment

import Control.Concurrent.STM
import Control.Concurrent.Async

import Data.Connection

import Network.Socket hiding (close)

import System.IO.Streams.TCP (TCPConnection)

import qualified System.IO.Streams     as SR
import qualified System.IO.Streams.TCP as TCP

import System.IO.Streams.Binary

import Data.Time
import Data.Char

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import qualified Data.Attoparsec.Text as AT

import ChatMessage

import Data.Scientific

import Control.Exception

{-
main :: IO ()
main = do
    { hSetEncoding stdin  utf8
    ; hSetEncoding stdout utf8
    ; hSetEncoding stderr utf8
    ; (adr:pnum':_) <- getArgs
    ; let pnum = read pnum'
    ; 
    }
-- asdfzxcv
-}

-- taken from SampleClient.hs and modified
-- hmm...

main :: IO ()
main = do
    { hSetEncoding stdin  utf8
    ; hSetEncoding stdout utf8
    ; hSetEncoding stderr utf8
    ; (srv:pn:_) <- getArgs
    ; let pnum = (read pn) :: PortNumber
    ; con1 <- TCP.connect srv pnum
    ; npn  <- fromIntegral <$> fromJust <$> (getFromStream getWord16le $ source con1)
    ; close con1
    ; conn <- TCP.connect srv npn
    ; tqin <- newTQueueIO
    ; tqtx <- newTQueueIO
    ; putStrLn "Type \"$<your_username>\" to assign yourself a username."
    ; putStrLn "Note that spaces are not allowed in usernames; only the first word will be counted as your username."
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
--  ; print x
    ; queuerThread tq conn
    }
-- asdfzxcv

-- Take input from the user, and write it to a queue.
-- (The hardest part to adapt)
writerThread :: TQueue ClientMessage -> IO ()
writerThread tq = do
    { txt <- T.getLine
    ; let msg = AT.parseOnly parseCommand txt
    ; case msg of
        { (Right ClientExit) -> do
            { atomically $ writeTQueue tq ClientExit
            }
        ; (Right msg') -> do
            { atomically $ writeTQueue tq msg'
            ; writerThread tq
            }
        ; (Left _) -> do
            { putStrLn "Error: Could not parse text."
            ; writerThread tq
            } 
        }
    }
-- asdfzxcv

parseCommand :: AT.Parser ClientMessage
parseCommand = AT.peekChar >>= parseCommand'
-- asdfzxcv

parseCommand' :: (Maybe Char) -> AT.Parser ClientMessage
parseCommand' (Just '#') = do
    { AT.anyChar -- consume the '#'
    ; num <- toBoundedInteger <$> AT.scientific
    ; n <- case num of
        { Nothing  -> fail "oops."
        ; (Just x) -> return x -- $ AT.takeWhile (\= '\n')
        }
    ; AT.skipSpace
    ; txt <- AT.takeWhile (/= '\n')
    ; return $ ClientPrivate n txt
    } <|> do
    { txt <- AT.takeWhile (/= '\n')
    ; return $ ClientBroadcast txt
    }
parseCommand' (Just '?') = do
    { AT.anyChar   -- consume the '?'
    ; AT.skipSpace
    ; strt <- (maybe (fail "oops") return) =<< toBoundedInteger <$> AT.scientific
    ; AT.skipSpace
    ; nums <- (maybe (fail "oops") return) =<< toBoundedInteger <$> AT.scientific
    ; AT.skipSpace
    ; x    <- AT.peekChar
    ; unless (x == Nothing) (fail "oops.")
    ; return $ ClientBrowse strt nums
    } <|> do
    { txt <- AT.takeWhile (/= '\n')
    ; return $ ClientBroadcast txt
    }
parseCommand' (Just 'q') = do
    { "quit" *> AT.skipSpace
    ; x <- AT.peekChar
    ; case x of
        { Nothing   -> return ClientExit
        ; Just '\0' -> return ClientExit
        ; _         -> fail "oops."
        }
    } <|> do
    { txt <- AT.takeWhile (/= '\n')
    ; return $ ClientBroadcast txt
    }
parseCommand' (Just '$') = do
    { AT.anyChar -- consume the '$'
    ; AT.skipSpace
    ; nm <- AT.takeWhile (not . isSpace)
    ; return $ ClientSetup nm
    }
parseCommand' (Just 'e') = do
    { "exit" *> AT.skipSpace
    ; x <- AT.peekChar
    ; case x of
        { Nothing   -> return ClientExit
        ; Just '\0' -> return ClientExit
        ; _         -> fail "oops."
        }
    } <|> do
    { txt <- AT.takeWhile (/= '\n')
    ; return $ ClientBroadcast txt
    }
parseCommand' _ = ClientBroadcast <$> (AT.takeWhile (/= '\n'))
-- sadfxzcv


-- Examine the queues of data from the server and the client, and
-- process/send messages from them respectively.
mainThread :: TCPConnection -> TQueue ServerMessage -> TQueue ClientMessage -> IO ()
mainThread conn tqin tqout = do
 -- { rslt <- race (atomically $ readTQueue tqin) (atomically $ readTQueue tqout)
    { rslt <- atomically $ raceSTM (readTQueue tqin) (readTQueue tqout)
    ; case rslt of
        { (Left  smsg) -> case smsg of
            { (ServerChat usr uid msg tim) -> do
                { tim' <- utcToLocalZonedTime tim
                ; T.putStrLn $ usr <> " (" <> (T.pack $ show uid) <> ") @ " <> (T.pack $ show tim') <> " :\n" <> msg
                ; mainThread conn tqin tqout
                }
            ; (ServerPrivate usr uid msg tim) -> do
                { tim' <- utcToLocalZonedTime tim
                ; T.putStrLn $ "Private message from " <> usr <> " (" <> (T.pack $ show uid) <> ") @ " <> (T.pack $ show tim') <> " :\n" <> msg
                ; mainThread conn tqin tqout
                }
            ; (ServerAlert msg tim) -> do
                { tim' <- utcToLocalZonedTime tim
                ; T.putStrLn $ "(" <> (T.pack $ show tim') <> ") : " <> msg
                ; mainThread conn tqin tqout
                }
            ; (ServerSetup uid) -> do
                { putStrLn $ "Connected to server with user id #" ++ (show uid) ++ "."
                ; mainThread conn tqin tqout
                }
            ; (ServerUserList usrs) -> do
                { mapM_ (\(uid,unm) -> T.putStrLn $ (T.pack $ show uid) <> " : " <> unm) usrs
                ; mainThread conn tqin tqout
                }
            ; (ServerError err) -> do
                { T.putStrLn $ "Error: " <> err
                ; mainThread conn tqin tqout
                }
            ; (ServerExit) -> do
                { putStrLn $ "Server is exiting."
                }
            }
        ; (Right cmsg) -> case cmsg of
            { (ClientExit) -> do
                { send conn $ encode ClientExit
                }
            ; climsg -> do
                { send conn $ encode climsg
                ; mainThread conn tqin tqout
                }
            }
        }
    }
-- asdfzxcv


-- Not really race, but something like it.
-- Really just get the first piece of data
-- from one of two queues, with a left bias.
raceSTM :: STM a -> STM b -> STM (Either a b)
raceSTM sta stb = (Left <$> sta) `orElse` (Right <$> stb)



