-- For servers on public IP addresses

{-|
Module      : Server.TCP.Public
Description : A server that automatically opens new ports
Copyright   : David Wilson, 2021
License     : BSD-3
Stability   : Unknown
Portability : Unknown

This is a server that automatically opens and closes
ports as needed. You set up an initial listening port
that all clients connect to, and then send a new port
number through this connection. The client connects
to this new port number, for actual communications.

The maximum number of connections for each 'communication' port
number is defined by the user. This library automatically
opens up a new port number when all communication ports are already
in use. It also closes any communication ports that have no active
connections. This is enforced using a modified semaphore that can
also be checked for when all 'resources' are available.

Note that this module only works on
  * Servers with a public IP address.
  * Local networks with local reach.

If you require NAT traversal, you'll have to wait until
I figure out how to do that in Haskell. If I do, it will
be in a module titled Server.TCP.Private.

-}

module Server.TCP.Public
    ( -- * Type(s)
      PortTable
      -- * Initialisation
    , startServer
    , runAsyncServerSimple
    , runAsyncServerWithResource
    , acceptConnAsync
    , acceptConn
      -- * Low-level usage
    , choosePort
    ) where
-- asdfzxcv

import Control.Exception

import qualified Data.ByteString.Lazy as BL

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Types.PortTable

import Control.Concurrent.STM
import Control.Concurrent.Async

import Data.Connection

-- Can still use close', which works
-- very similarly to close.
import Network.Socket hiding (close)

import qualified System.IO.Streams     as SR
import qualified System.IO.Streams.TCP as TCP

import System.IO.Streams.TCP (TCPConnection)

-- How it works:

-- The server has one fixed 'listen' port where it listens for
-- new connections. It then sends a new port number on request,
-- which is the main port for communication. Each port has a 
-- maximum number of connections, and the port table handles
-- these connections.

-- | Accept a connection on the listen port, and
-- then choose a port to listen to, possibly
-- opening a new port.
choosePort :: Async () -> TCPConnection -> PortTable Socket -> (TCPConnection -> IO a) -> IO a
choosePort dummy conn pt fio = do
    -- This automatically increments the counter of the port/socket.
    { pn <- atomically $ getPort pt
    ; case pn of
        { (Just (pnum,sock,tv)) -> do
            -- Should really put these two lines in the 'setup' part of
            -- the bracket below.
            { send  conn (runPut $ putWord16le $ fromIntegral pnum)
            ; close conn --
            -- "TCP.accept sock" probably shouldn't be in the setup phase...
            -- It might be okay, since if TCP.accept blocks then it *should* be
            -- cancellable even if it is masked.
         -- ; bracket (TCP.accept sock) (\nconn -> close nconn >> putStrLn "closing a connection." >> atomically (signalTVar tv)) fio
         
            -- To show that each connection is on the same port number.
         -- ; let accept' sck = do { conn <- TCP.accept sck ; pn' <- socketPort $ fst $ connExtraInfo conn ; putStrLn $ (show pn') ++ " == " ++ (show pnum) ++ "?" ; return conn }
         -- ; bracket (    accept' sock) (\nconn -> close nconn >> atomically (signalTVar tv)) fio
            
            -- Otherwise...
            ; bracket (TCP.accept  sock) (\nconn -> close nconn >> atomically (signalTVar tv)) fio
            }
        -- Need to open a new port
        ; Nothing -> do
            { sock <- TCP.bindAndListen (defaultConnLimit pt) defaultPort
            -- Risky... 
            -- (Could use socketPortSafe, but then what?)
            ; pnum <- socketPort sock
            ; tv   <- atomically $ do 
                { tv <- newPortVal pt pnum sock
                ; waitTVar tv -- doesn't actually wait; just decrements counter
                ; return tv
                }
                
            -- Debug...
         -- ; putStrLn $ "Opening socket on port " ++ (show pnum) ++ "."
                
            -- Very risky (to create a thread with async directly).
            -- To avoid that, I run a dummy thread with withAsync,
            -- and then link any threads created with async to the
            -- dummy thread.
            
            -- This ~shouldn't~ fail, since the connections are automatically closed on exception,
            -- but it's still pretty uncouth(?).
            -- linkOnly (const True) dummy
            ; asy   <- async $ bracketOnError ( linkOnly (const True) dummy)
                                              (\_ -> (putStrLn $ "closer for port " ++ (show pnum) ++ " received exception.") >> (atomically $ closePort' pt pnum sock tv (defaultConnLimit pt)) >> (putStrLn $ "closer for port " ++ (show pnum) ++ "finished closing stuff (in exception)") )
                                              (\_ -> (atomically $ closePort' pt pnum sock tv (defaultConnLimit pt)) >> (putStrLn "finished closing stuff (not in exception)") )   
            -- To toggle debug messages easily.
            
         -- ; async $ bracketOnError ( linkOnly (const True) dummy)
         --                          (\_ -> atomically $ closePort' pt pnum sock tv (defaultConnLimit pt)) -- run on exceptions
         --                          (\_ -> atomically $ closePort' pt pnum sock tv (defaultConnLimit pt)) -- main action
            
         -- ; asy   <- async ((atomically $ closePort' pt pnum sock tv (defaultConnLimit pt))  `onException` (atomically $ closePort' pt pnum sock tv (defaultConnLimit pt)) )
            -- (The above line(s) write[s] the socket to the "close queue" when it doesn't have any active connections)
         -- ; putStrLn $ "Opened a new thread : " ++ (show $ asyncThreadId asy)
            
            ; send conn (runPut $ putWord16le $ fromIntegral pnum)
            ; close conn
         -- ; nconn <- TCP.accept sock
         -- ; (fio nconn) `finally` ( close nconn >> atomically (signalTVar tv))
            -- bracket :: (IO a) -> (a -> IO b) -> (a -> IO c) -> IO c
            -- ...I think...
         -- ; bracket (TCP.accept sock) (\nconn -> close nconn >> putStrLn "closing a connection" >> atomically (signalTVar tv)) fio
            ; bracket (TCP.accept sock) (\nconn -> close nconn >> atomically (signalTVar tv)) fio
            }
        }
    }
-- asdfzxcv

-- | A convenience function for if you just want to immediately
-- change to the new port.
acceptConn :: Async () -> Socket -> PortTable Socket -> (TCPConnection -> IO a) -> IO a
acceptConn dummy sock pt fio = do
    { conn <- TCP.accept sock
--  ; withAsync (dummyThread) (\asy -> choosePort asy conn pt fio)
    ; choosePort dummy conn pt fio
    }
-- asdfzxcv

-- | Repeatedly accept connections and run the same function on them.
acceptConnAsync :: (TCPConnection -> IO a) -> (Async ()) -> Socket -> PortTable Socket -> IO ()
acceptConnAsync fio dummy sock pt = do
    { conn <- TCP.accept sock
    ; withAsync (choosePort dummy conn pt fio) (\_ -> acceptConnAsync fio dummy sock pt)
    }
-- asdfxzcv

-- | Combines 'startServer' and 'acceptConnAsync' into one, slightly easier to use function.
-- This is probably the function you want.
runAsyncServerSimple :: PortNumber -> Int -> Int -> (TCPConnection -> IO a) -> IO ()
runAsyncServerSimple pnum dcl lcl fio = startServer pnum dcl lcl (acceptConnAsync fio)

-- Be sure to call this using 'withAsync';
-- if you call it with 'async', it will never
-- end.
closeSocks :: TQueue Socket -> IO ()
closeSocks tq = do
    { sock <- atomically $ readTQueue tq
    ; pnum <- socketPortSafe sock
    -- For debug
    ; case pnum of
        { (Just pn) -> putStrLn $ "Closing socket on port " ++ (show pn) ++ "."
        ; Nothing   -> putStrLn $ "Closing a socket..."
        }
    ; close' sock
    ; closeSocks tq
    }
-- asdfxzcv


-- pnum' : The port number of the socket listening for connections (can be 0).
-- dcl   : The default connection limit per socket.
-- lcl   : The connection limit for the listening port.
-- fio   : The function to run the actual server.
-- startServer :: PortNumber -> Int -> Int -> (Socket -> PortTable Socket -> IO a) -> IO a

-- | Start a server, using a function on the socket and port table
-- to run the server.
startServer :: PortNumber                                       -- ^ The port number of the socket listening for connections (can be 0).
            -> Int                                              -- ^ The default connection limit per socket.
            -> Int                                              -- ^ The connection limit for the listening port.
            -> (Async () -> Socket -> PortTable Socket -> IO a) -- ^ The function to run the actual server.
            -> IO a                                             -- ^ The return type
startServer pnum' dcl lcl fio = do
    -- note that newPortTable automatically sets the dcl
    -- to 1 if the supplied value is 0 or lower.
    { pt   <- atomically $ newPortTable dcl
    -- TODO: Add checking to make sure the port can
    -- be used, along with an option on what to do if
    -- it can't.
    ; sock <- TCP.bindAndListen (max 2 lcl) pnum'
    -- Gets the actual port number if 'defaultPort'
    -- was specified.
    ; pnum <- case pnum' of
        { x | x == defaultPort -> do
            { pn <- socketPort sock
            ; putStrLn $ "Listen Port = " ++ (show pn)
            ; return pn
            }
        ; x -> return x
        }
    -- TODO: Maybe enclose the upper part in the 'setup'
    -- part of a bracket?
    
    -- As noted above, closeSocks HAS to be run with 'withAsync',
    -- since it would never terminate otherwise.
    ; withAsync (closeSocks (emptySockets pt)) $ \_ -> do
      -- { withAsync (handleCloserThreads' pt)   (\_ -> (fio sock pt) `finally` (close' sock))
         { withAsync (dummyThread) (\dummy -> (fio dummy sock pt) `finally` (close' sock))
         -- Possibly some more stuff here.
         }
    }
-- asdfzxcv

-- Methods based on the old version of ensuring threads
-- would close.

-- Remove completed threads from the set of open threads.
{-
handleCloserThreads :: PortTable Socket -> IO ()
handleCloserThreads pt = do
    { atomically $ remClosedThreads pt
    ; handleCloserThreads pt
    }
-- asdfzxcv

handleCloserThreadsOnce :: PortTable Socket -> IO ()
handleCloserThreadsOnce pt = do
    { x <- atomically $ remClosedThreadsOnce pt
    ; if x then (handleCloserThreadsOnce pt) else (return ())
    }
-- asdfzxcv

handleCloserThreads' :: PortTable Socket -> IO ()
handleCloserThreads' pt = (handleCloserThreads pt) `onException` ((handleCloserThreadsOnce pt) >> (cancelOpenThreads pt))
-}

---------------------
-- Newer Functions --
---------------------

-- runAsyncServerSimple pnum dcl lcl fio = startServer pnum dcl lcl (acceptConnAsync fio)

-- | Create a shared resource that all connections can access.
-- 
runAsyncServerWithResource :: (IO a)                       -- ^ Acquire resource
                           -> (a -> IO b)                  -- ^ Release resource
                           -> PortNumber                   -- ^ Initial port number
                           -> Int                          -- ^ Connection limit for each communication port.
                           -> Int                          -- ^ Connection limit for the listen port.
                           -> (a -> TCPConnection -> IO c) -- ^ The function to run on each connection.
                           -> IO ()                        
runAsyncServerWithResource strt endr pnum dcl lcl fio = bracket strt endr (\x -> runAsyncServerSimple pnum dcl lcl (fio x))


-- Create a thread that always blocks.
-- This is useful to spawn as the 'action' part
-- of a call to withAsync, so that you can link
-- other threads to it. Then, in theory, those threads
-- will be cancelled when this thread is cancelled.
-- Example Usage:
--
-- > withAsync dummyThread $ \dummy -> do
-- >     { async (linkOnly (const True) dummy >> action1)
-- >     ; async (linkOnly (const True) dummy >> action2)
-- >     ; async (linkOnly (const True) dummy >> action3)
-- >     ; action4
-- >     }
--
-- In theory, action1, action2, and action3 will be cancelled
-- once action4 completes. Thus, this allows you to (in theory)
-- spawn multiple threads that all have a predetermined endpoint,
-- but not-yet-determined creation points. 
dummyThread :: IO ()
dummyThread = do
    { tv <- newTVarIO False
    ; atomically $ do
        { b <- readTVar tv
        ; check b
        }
--  ; return () -- unnecessary; check returns ().
    } `finally` (putStrLn "dummy has ended.")
-- asdfzxcv

-- see https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.15.0.0/src/Control-Exception-Base.html#bracketOnError
-- bracketOnErrorAlt :: (Exception e) => IO a -> (e -> a -> IO b) -> (a -> IO c) -> IO c
-- bracketOnErrorAlt

-- onExceptionAlt :: (Exception e) => IO a -> (e -> IO b) -> IO a
-- onExceptionAlt io what = io `catch` \e -> do { _ <- what e ; throwIO (e :: SomeException) }

{-
-- Bracket that allows exceptions to be caught in the 'setup' phase.
bracketAlt :: IO a -> IO d -> (a -> IO b) -> (a -> IO c) -> IO c
bracketAlt before bAfter after thing = mask $ \restore -> do
    { a <- restore  before   `onException` bAfter
    ; r <- restore (thing a) `onException` after a
    ; _ <- after a
    ; return r
    }
-- asdfzxcv

-- Like bracketAlt, but without an action to close
-- the 'before' action.
-- Most useful as a drop-in replacement for bracket where
-- the acquire action doesn't need to be (or shouldn't) be
-- masked.
bracketAlt' :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracketAlt' = mask $ \restore -> do
    { a <- retore before
    ; r <- restore (thing a) `onException` after a
    ; _ <- after a
    ; return r
    }
-- asdfxzcv
-}
