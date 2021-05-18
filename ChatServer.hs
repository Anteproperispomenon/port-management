{-# LANGUAGE OverloadedStrings #-}

module Main where

import Server.TCP.Public

import Types.TCP.Monad

import ChatMessage

import UserList

import Data.Time

import Data.Word

import Control.Monad
import Control.Monad.Catch
import Control.Concurrent.STM

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import qualified Data.Text as T

import System.IO
import System.Environment

main :: IO ()
main = do 
    { hSetEncoding stdin  utf8
    ; hSetEncoding stdout utf8
    ; hSetEncoding stderr utf8
    ; (pnum':_) <- getArgs
    ; let pnum = read pnum'
    ; runAsyncServerWithResource (atomically $ createUniqueUserList) (\_ -> return ()) pnum 5 5 (runTCP (runThread `onException` (sendBinary ServerExit)) getClientMessage)
    }
-- asdfzxcv

-- For before the user has been assigned a username.
runThread :: (UserList userlist) => TCPT ClientMessage (userlist ServerMessage) IO ()
runThread = do
    { msg <- recv
    ; case msg of
        { (ClientSetup usrnm) -> do
            { usrlst <- getResource
            ; rslt   <- atomicallyTCPT (insertUser usrlst usrnm)
            ; case rslt of
                { (Right (uid,tq)) -> do
                    -- Now change to the other function
                    { sendBinary (ServerSetup uid)
                    ; tim <- liftIO getCurrentTime
                    ; atomicallyTCPT $ broadcastMessage usrlst (ServerAlert ("User \"" <> usrnm <> "\" has joined the chat.") tim) 
                    ; runUser usrnm uid tq
                    }
                ; (Left txt)       -> do
                    { sendBinary (ServerError txt)
                    ; runThread
                    }
                }
            }
        ; (ClientBrowse strt num) -> do
            { usrlst <- getResource
            ; usrs   <- atomicallyTCPT (getUserList usrlst strt num)
            ; sendBinary (ServerUserList usrs)
            ; runThread
            }
        ; (ClientExit) -> return () -- Exits.
        ; _ -> do
            { sendBinary (ServerError "You can't perform that action until you have a username.")
            ; runThread
            }
        }
    }
-- asdfzxcv


runUser :: (UserList userlist) => T.Text -> Word64 -> TQueue ServerMessage -> TCPT ClientMessage (userlist ServerMessage) IO ()
runUser usnm uid tq = do
    { rslt <- recvOrElseEither (readTQueue tq)
    -- Breaking it into two layers of case-statements
    -- to avoid writing (Left (Server...)) each time.
    ; case rslt of
        { (Left srvmsg)  -> case srvmsg of
            { (ServerExit) -> do
                { sendBinary ServerExit
                }
            ; msg -> do
                { sendBinary msg
                ; runUser usnm uid tq
                }
            }
        ; (Right climsg) -> case climsg of
            { (ClientBroadcast msg) -> do
                { tim <- liftIO $ getCurrentTime
                ; usl <- getResource
                ; atomicallyTCPT $ broadcastMessage usl (ServerChat usnm uid msg tim)
                ; runUser usnm uid tq
                }
            ; (ClientPrivate rid msg) -> do
                { tim <- liftIO $ getCurrentTime
                ; usl <- getResource
                ; atomicallyTCPT $ sendMessage usl rid (ServerPrivate usnm uid msg tim)
                ; runUser usnm uid tq
                }
            ; (ClientBrowse strt num) -> do
                { usrlst <- getResource
                ; usrs   <- atomicallyTCPT (getUserList usrlst strt num)
                ; sendBinary (ServerUserList usrs)
                ; runUser usnm uid tq
                }
            ; (ClientExit)    -> do
                { usrlst <- getResource
                ; tim    <- liftIO $ getCurrentTime
                ; atomicallyTCPT $ remUser usrlst uid
                ; atomicallyTCPT $ broadcastMessage usrlst (ServerAlert ("User \"" <> usnm <> "\" has left the chat.") tim)
                }
            ; (ClientSetup _) -> do
                { sendBinary $ ServerError "Can't change username. Feature not yet implemented."
                ; runUser usnm uid tq
                }
            }
        }
    }
-- asdfxzcv












