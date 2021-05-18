{-# LANGUAGE OverloadedStrings #-}

module UserList 
    ( UserList(..)
    , UniqueUserList
    , createUniqueUserList
    ) where
-- asdfzxcv

import Control.Concurrent.STM

import qualified StmContainers.Bimap as BM
import qualified StmContainers.Map   as SM

import qualified ListT as LT

import qualified DeferredFolds.UnfoldlM as UF

import qualified Focus as FC

-- Use Bimap for when usernames must be unique
-- Use map for when they don't have to be.

-- At the moment, only unique user lists are implemented.

import qualified Data.Text as T

import Data.Word

-- import ChatMessage

class UserList a where
    -- Returns an error or their new id.
    insertUser       :: (a msg) -> T.Text -> STM (Either T.Text (Word64, TQueue msg))
    remUser          :: (a msg) -> Word64 -> STM ()
    createUserList   :: STM (a msg)
    sendMessage      :: (a msg) -> Word64 -> msg -> STM ()
    broadcastMessage :: (a msg)           -> msg -> STM ()
    getUserList      :: a msg -> Int -> Int -> STM [(Word64,T.Text)]
-- asdfzxcv

instance UserList UniqueUserList where
    insertUser       = uniqueInsertUser
    remUser          = uniqueRemUser
    createUserList   = createUniqueUserList
    sendMessage      = uniqueSendMessage
    broadcastMessage = uniqueBroadcastMessage
    getUserList      = uniqueGetList
-- asdfzxcv

-- pattern:
-- insertUser = do 
--   x    <- readTVar counter
--   rslt <- tryInsertUser lst x nm
--   case rslt of
--        (Right ...) -> do { modifyTVar (+1) counter ; 



data UniqueUserList msg
   = UniqueUserList { uniqMap :: BM.Bimap Word64 T.Text
                    , uniqCtr :: TVar Word64
                    , uniqMsg :: SM.Map Word64 (TQueue msg)
                    }
-- asdfzxcv

createUniqueUserList :: STM (UniqueUserList msg)
createUniqueUserList = UniqueUserList <$> BM.new <*> (newTVar 2) <*> SM.new

uniqFocusInsert :: Word64 -> FC.Focus Word64 STM Bool
uniqFocusInsert uid = FC.cases (True, FC.Set uid) (\_ -> (False, FC.Leave))

uniqueInsertUser :: UniqueUserList msg -> T.Text -> STM (Either T.Text (Word64, TQueue msg))
uniqueInsertUser (UniqueUserList mp ctr mp2) usrnm = do
    { uid  <- readTVar ctr
    ; rslt <- BM.focusRight (uniqFocusInsert uid) usrnm mp
    ; if rslt
        then do
          { modifyTVar ctr (+1)
          ; tq <- newTQueue
          ; SM.insert tq uid mp2
          ; return $ Right (uid, tq)
          }
        else (return $ Left ("The username \"" <> usrnm <> "\" has already been taken."))
    }
-- asdfxzcv

uniqueRemUser :: UniqueUserList msg -> Word64 -> STM ()
uniqueRemUser (UniqueUserList mp ctr mp2) uid = do
    { BM.deleteLeft uid mp
    ; SM.delete     uid mp2
    }
-- asdfzxcv

uniqueSendMessage :: UniqueUserList msg -> Word64 -> msg -> STM ()
uniqueSendMessage (UniqueUserList _ _ mp2) uid msg = do
    { mtq <- SM.lookup uid mp2
    ; case mtq of 
        { (Just tq) -> writeTQueue tq msg
        ; Nothing   -> return ()
        }
    }
-- asdfzxcv


-- use LT.traverse_ :: Monad m => (a -> m ()) -> ListT m a -> m ()
-- SM.listT :: SM.Map key value -> ListT STM (key, value)
uniqueBroadcastMessage :: UniqueUserList msg -> msg -> STM ()
uniqueBroadcastMessage (UniqueUserList _ _ mp2) msg
    = LT.traverse_ (\(uid,tq) -> writeTQueue tq msg) (SM.listT mp2)
-- asdfzxcv


uniqueGetList :: UniqueUserList msg -> Int -> Int -> STM [(Word64,T.Text)]
uniqueGetList (UniqueUserList mp _ _) strt num
    = LT.toList $ LT.take num $ LT.drop strt $ BM.listT mp
-- asdfzxcv

