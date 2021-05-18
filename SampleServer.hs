{-# LANGUAGE OverloadedStrings #-}

--

module Main where

import Server.TCP.Public

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
import qualified Data.Text.Lazy.Encoding as T

import Types.SampleMessage

import Network.Socket hiding (close)

import Control.Concurrent.STM
import Control.Concurrent.Async

import Control.Exception

main :: IO ()
main = do
    { [pn] <- getArgs
    ; let pnum = (read pn) :: PortNumber
    ; startServer pnum 3 10 (acceptConnAsync simpleAction')
    }
-- asdfzxcv

-- Initiates the action, but send an exit message to the
-- client if it encounters an exception.
simpleAction' :: TCPConnection -> IO ()
simpleAction' conn = (simpleAction conn) `onException` (send conn $ encode ServerExit)

-- Decode messages from the client, and act on them appropriately.
simpleAction :: TCPConnection -> IO ()
simpleAction conn = do
    { rslt <- decodeFromStream $ source conn
    ; case rslt of
        { Nothing                 -> do 
            { send conn $ encode (ServerText "Error: Couldn't decode message.")
            ; simpleAction conn
            }
        ; (Just (ClientText txt)) -> do
            { send conn $ encode (ServerText $ T.reverse txt)
            ; simpleAction conn
            }
        ; (Just ClientExit)       -> do
         -- { send conn $ encode ServerExit -- This causes headaches.
            { putStrLn "Ending connection..."
            }
        }
    }
-- asdfzxcv






