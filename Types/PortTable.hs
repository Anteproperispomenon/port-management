-- Haskell Port Management
-- This does not handle new ports/etc...
-- It is a port-opening-method-agnostic interface.

module Types.PortTable
    ( PortTable(..)
    , newPortVal
    , newPortValLim
    , newPortTable
    , getPort
    , waitTVar
    , signalTVar
    , closePort
    , closePort'
 -- , addOpenThread
 -- , remClosedThreads
 -- , remClosedThreadsOnce
 -- , cancelOpenThreads
    ) where
-- asdfzxcv

import Data.Hashable

import Data.Word
import Data.Bits

import Data.Functor

import Control.Monad
import Control.Applicative

import Control.Concurrent.STM
import Control.Concurrent.Async

import qualified StmContainers.Map as SM
-- import qualified StmContainers.Set as SS

import Network.Socket (PortNumber(..))

import qualified DeferredFolds.UnfoldlM as UF
import qualified DeferredFolds.Unfoldr  as UR
import qualified Control.Foldl as FL

import qualified ListT as LT

-- Since Semaphores are just limited TVars, I'm
-- using TVars instead.


-- The 'a' is most likely a socket.

-- | A table that contains a mapping from PortNumbers to Sockets
-- and semaphores.
data PortTable a
   = PortTable { defaultConnLimit :: Int
               , getPortTable     :: SM.Map PortNumber (a, Int, (TVar Int))
               , emptySockets     :: TQueue a -- Sockets that need to be closed.
               -- Hard to know where to implement this thing.
            -- , closingThreads   :: SS.Set (Async ())
               }
-- asdfzxcv

newPortTable :: Int -> STM (PortTable a)
newPortTable mx = PortTable mx' <$> SM.new <*> newTQueue -- <*> SS.new
    where mx' = max mx 1
-- asdfxzcv

-- Taken from Data.Hashable
instance Hashable PortNumber where
    hash = fromIntegral
    hashWithSalt = hashUsing (fromIntegral :: PortNumber -> Word16)
--  hashWithSalt salt x = salt
--      where combine h1 h2 = (h1 * 1677619) `xor` h2
-- asdfxzcv

newPortVal :: PortTable a -> PortNumber -> a -> STM (TVar Int)
newPortVal (PortTable dcl mp tq) pnum val = do
    { tv <- newTVar dcl
    ; SM.insert (val,dcl,tv) pnum mp
    ; return tv
    }
-- asdfzxcv 

-- With a non-default connection limit.
newPortValLim :: PortTable a -> PortNumber -> a -> Int -> STM (TVar Int)
newPortValLim (PortTable _ mp _) pnum val cl
    | (cl <= 0) = fail "Error: Can't create port with connection limit <= 0."
    | otherwise = do
        { tv <- newTVar cl
        ; SM.insert (val,cl,tv) pnum mp
        ; return tv
        }
-- asdfxzcv

-- Makes sure no one is using a port when
-- it is closed
closePort :: PortTable a -> PortNumber -> STM ()
closePort (PortTable dcl mp tq) pnum = do
    { myb <- SM.lookup pnum mp
    ; case myb of
        { (Just (sock,mx,tv)) -> do
            { num <- readTVar tv
            -- Retry if there's someone using it.
            ; check (num >= mx)
            ; SM.delete pnum mp
            ; writeTVar tv 0 -- Just to be extra safe.
            ; writeTQueue tq sock
            }
        ; Nothing -> return ()
        }
    }
-- asdfzxcv

-- In case the other one wakes up any time 
-- the map is modified.
closePort' :: PortTable a -> PortNumber -> a -> TVar Int -> Int -> STM ()
closePort' (PortTable dcl mp tq) pnum sock tv mx = do
    { num <- readTVar tv
    -- Retry if there's someone using it.
    ; check (num >= mx)
    ; SM.delete pnum mp
    ; writeTVar tv 0 -- Just to be extra safe.
    ; writeTQueue tq sock
    }
-- asdfzxcv

-- Closing the port in the event of an error.
-- (to be used with `onException`)
-- closePortError :: PortTable a -> PortNumber -> a -> TVar Int -> Int -> STM ()
-- closePortError (PortTable dcl mp tq _) pnum sock tv mx = do

tryPort :: (TVar Int) -> PortNumber -> a -> STM (Maybe (PortNumber,a,TVar Int))
tryPort sem pnum sck = waitTVar sem $> (Just (pnum,sck,sem))

-- findPort :: PortTable a -> PortNumber -> STM (Maybe a)
-- findPort (PortTable _ mp _) 

-- getPort :: PortTable a -> STM (Maybe (PortNumber,a))
-- getPort (PortTable _ mp _)

-- This is really complicated...
-- Might switch to use ListT instead later...
getPort :: (PortTable a) -> STM (Maybe (PortNumber,a,TVar Int))
getPort (PortTable _ mp _) = do
    { let ufl = SM.unfoldlM mp
    ; (join $ UF.foldlM' (\o (pnum,(x,_,tv)) -> return (o <|> (tryPort tv pnum x))) retry ufl) <|> (return Nothing)
    }
-- asdfzxcv

-- How it works:
-- Since the function we want to fold on (<|>) requires that both arguments
-- be enclosed in a monad, we have to enclose the output in two monads.
-- This is because foldlM' 'extracts' the value from the outtermost monad
-- layer when passing it to the folding function. However, the final output
-- is (m output), i.e. foldlM' doesn't extract it for the final step. Thus,
-- we have to use join to combine the two layers at the end.

-- Note that the initial value is retry. This is because left folds start
-- with the initial value, and thus it has to be something that causes the
-- computation to pass right through. 'retry' is the only option.

-- Also, the final (return Nothing) is so that if all of the ports are full,
-- the function still returns (otherwise it would retry the whole thing all
-- over again).


-- Important functions:
-- FL.generalize
-- SM.unfoldlM

-- FL.premap (\(pn,(sck,_,tv)) -> (pn,sck)) $


-- The fold:
-- (Just <$> tryPort x1) <|> (Just <$> tryPort x2) <|> ... <|> (return Nothing)


-- unfoldlM :: Map k v -> UnfoldlM STM (k,v)
-- unfoldlM :: Map PortNumber (a,Int,Tvar Int) -> UnfoldlM STM (PortNumber,(a,Int,Tvar Int))



------------------------------
-- Semaphore-like Functions --
------------------------------

-- Taken from
-- http://hackage.haskell.org/package/stm-2.5.0.0/docs/src/Control.Concurrent.STM.TSem.html#TSem

waitTVar :: (TVar Int) -> STM ()
waitTVar t = do
  i <- readTVar t
  when (i <= 0) retry
  writeTVar t $! (i-1)
-- asdfzxcv

signalTVar :: (TVar Int) -> STM ()
signalTVar t = do
  i <- readTVar t
  writeTVar t $! i+1
-- asdfzxcv

-- setToList :: SS.Set v -> STM [v]
-- setToList st = LT.toList $ SS.listT st
-- setToList st = do
--  { let ufl = SS.unfoldlM st
--  ; UF.foldlM' (\o i -> return $ (i:o)) [] ufl
--  }
-- asdfzxcv

-- Pretty Basic
-- addOpenThread :: PortTable a -> Async () -> STM ()
-- addOpenThread (PortTable _ _ _ st) asy = SS.insert asy st

{-
-- See https://stackoverflow.com/questions/18108608/what-are-alternatives-some-and-many-useful-for
-- for the use of some/many with STM.
-- many is used since the set of asyncs might change between runs.
-- Note that it only runs once;
-- to run it continuously, you'd have to enclose it in an IO block.
remClosedThreads :: PortTable a -> STM ()
remClosedThreads (PortTable _ _ _ st) = do
    { lst <- setToList st
    ; thd <- fst <$> waitAnySTM lst
    ; SS.delete thd st
    -- This won't work; waitAnySTM will just return the first
    -- completed thread *every* time, so it will repeat 
    -- endlessly.
--  ; thds <- map fst <$> many (waitAnySTM lst)
--  ; mapM_ (\x -> SS.delete x st) thds
    }
-- asdfzxcv
-}

{-
-- Remove all threads that have been completed, once.
-- This is useful to use in the close section of an
-- 'onException' or 'bracketOnException' block, if you
-- want to do something extra for threads that have not
-- yet completed.
-- 
remClosedThreadsOnce :: PortTable a -> STM Bool
remClosedThreadsOnce pt@(PortTable _ _ _ st) = do
    { lst  <- setToList st
    ; rslt <- (chng <$> waitAnySTM lst) <|> (return Nothing)
    ; case rslt of
        { (Just thd) -> do
            { SS.delete thd st
            ; return True
            }
        ; Nothing    -> return False
        }
    } where chng (x,_) = (Just x)
-- asdfzxcv
-}

{-
remClosedThreadsOnce' :: SS.Set (Async ()) -> [Async ()] -> STM ()
remClosedThreadsOnce' st lst = do
    { rslt <- (chng <$> waitAnySTM lst) <|> (return Nothing)
    ; case rslt of
        { (Just thd) -> do
            { SS.delete 
            }
        }
    } where chng (x,_) = (Just x)
-- asdfzxcv
-}

{-
cancelOpenThreads :: PortTable a -> IO ()
cancelOpenThreads (PortTable _ _ _ st) = do
    { lst <- atomically $ setToList st
    ; mapM_ cancelX lst
    } where cancelX thd = do { tid <- return $ asyncThreadId thd ; b <- cancelPoll thd ; when b (putStrLn $ "Cancelling thread " ++ (show tid))}
-- asdfxzcv
-- where cancelX thd = do {putStrLn $ "Cancelling thread " ++ (show $ asyncThreadId thd) ; cancel thd}
-}



-- A convenience function that polls whether an Async completed
-- or not, then cancels it. Only useful if you want to know
-- whether a thread was running when you cancelled it.
cancelPoll :: Async a -> IO Bool
cancelPoll asy = do
    { rslt <- poll asy
    ; case rslt of
        { Nothing  -> do { cancel asy >> return False}
        ; (Just _) -> return True
        }
    }
-- asdfxzcv


---------------------------------------
---------- Testing Functions ----------
---------------------------------------

-- Testing folds
-- foldl (>>) (Left "huh?") [Right "a", Right "b", Right "c", Left "d", Right "e"] == Left "huh?"
-- foldr (>>) (Left "huh?") [Right "a", Right "b", Right "c", Left "d", Right "e"] == Left "d"

-- UF.foldlM' (:) [] (UF.UnfoldlM (\f x -> foldM f x [1,2,3]))

-- UF.foldlM' (\x y -> [x+y]) 0 (UF.UnfoldlM (\f x -> foldM f x [1,2,3]))

unfoldrOn  :: [a] -> UR.Unfoldr a
unfoldrOn  xs = UR.Unfoldr (\f x -> foldr f x xs)

unfoldlMOn :: (Monad m) => [a] -> UF.UnfoldlM m a
unfoldlMOn xs = UF.UnfoldlM (\f x -> foldM f x xs)

-- UF.foldlM' (\x y -> Just (x+y)) 0 (UF.unfoldr (unfoldrOn [1..10]))
-- UF.foldlM' (\x y -> x <$ y) 0 (UF.unfoldr (unfoldrOn [Right "a", Right "b", Right "c", Left "d", Right "e"]))

-- basic tests
testMap :: STM (SM.Map Int String)
testMap = do
    { mp <- SM.new 
    ; SM.insert "hello" 0 mp
    ; SM.insert "world" 1 mp
    ; SM.insert "okay?" 2 mp
    ; SM.insert "help!" 3 mp
    ; return mp
    }
-- asdfzxcv

testOp :: STM String
testOp = do
    { mp <- testMap
    ; let ufl = SM.unfoldlM mp
    ; UF.foldlM' (\o (_,i) -> return $ o ++ i) "???" ufl
    }
-- asdfzxcv

-- To perform the necessary operations with a left-fold:
-- Make the initial value 'retry', and then surround
-- the entire fold with brackets, and then just do
-- (<insert fold here>) <|> (return Nothing)

-- This actually *reverses* the list,
-- which may be useful, since you'll probably want
-- to check newer ports first.
mapToList :: SM.Map k v -> STM [(k,v)]
mapToList mp = do
    { let ufl = SM.unfoldlM mp
    ; UF.foldlM' (\o i -> return $ (i:o)) [] ufl
    }
-- asdfzxcv

-- atomically (testMap >>= mapToList)

testMap2 :: STM (SM.Map PortNumber ((), Int, (TVar Int)))
testMap2 = do
    { mp <- SM.new
    ; t1 <- newTVar 0
    ; t2 <- newTVar 0
    ; t3 <- newTVar 0
    ; t4 <- newTVar 1
    ; t5 <- newTVar 0
    ; t6 <- newTVar 0
    ; SM.insert ((), 5, t1) 10000 mp
    ; SM.insert ((), 5, t2) 10001 mp
    ; SM.insert ((), 5, t3) 10002 mp
    ; SM.insert ((), 5, t4) 10003 mp
    ; SM.insert ((), 5, t5) 10004 mp
    ; SM.insert ((), 5, t6) 10005 mp
    ; return mp
    }
-- asdfzxcv

-- UF.foldlM' (\o i -> return $ (i:o)) [] ufl

-- THIS IS IT!!!
{-
tryTestMap :: (SM.Map PortNumber (a,Int,TVar Int)) -> STM (Maybe (PortNumber,a))
tryTestMap mp = do
    { let ufl = SM.unfoldlM mp
--  ; (join $ UF.foldlM' (\o (pn,(x,_,tv)) -> (return (o <|> (tryPort tv pn x))) retry ufl)) <|> (return Nothing)
    ; (join $ UF.foldlM' (\o (pn,(x,_,tv)) -> return (o <|> (tryPort tv pn x))) retry ufl) <|> (return Nothing)
    }
-- asdfzxcv
-}

-- (join $ UF.foldlM' (\o (pn,(x,_,tv)) -> return (o <|> (tryPort tv pn x))) retry ufl)

-- foldlM' :: Monad m => (output -> input -> m output) -> output -> UnfoldlM m input -> m output

-- output == STM (Maybe ...)



