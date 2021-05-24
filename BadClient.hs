{-# LANGUAGE OverloadedStrings #-}

-- A client that requests a connection,
-- but doesn't connect to the communication
-- port.

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

main = do
    { (srv:prt:_) <- getArgs
    ; let pnum = (read prt) :: PortNumber
    ; conn <- TCP.connect srv pnum
    ; npn  <- fromIntegral <$> fromJust <$> (getFromStream getWord16le $ source conn)
    ; putStrLn $ "New port number is " ++ (show npn)
    }
-- asdfzxcv



