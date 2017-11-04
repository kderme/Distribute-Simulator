{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module VProcess where

import Control.Monad.State
import Data.Map as M
import Data.Maybe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type VProcess a m b = StateT a m b

type ProcessId = Int

data (Show msg, Read msg, Eq msg) => Message msg = Message {
    from :: ProcessId
  , to   :: ProcessId
  , msg  :: msg
} deriving (Show, Eq)

-- | Class of types that act as states an so they have an initial default state.
class Stateable st where
  initState :: st

data (Show st, Show msg) => GlobalState st msg = GlobalState {
    states    :: M.Map ProcessId st
  , runningId :: ProcessId
  , nextId    :: ProcessId
  , sent      :: [Message msg]
  , received  :: [Message msg]
  } deriving (Show)

type VP st m msg v = VProcess (GlobalState st msg) m v

instance (Show st, Show msg) => Stateable (GlobalState st msg) where
  initState = GlobalState M.empty 0 1 [] []

getSelfPid :: (Monad m, Show st, Show msg) =>
              VProcess (GlobalState st msg) m ProcessId
getSelfPid = do
  GlobalState{..} <- get
  return runningId

say :: (Eq msg, Show msg, Read msg, Show st) =>
       String -> VProcess (GlobalState st msg) IO ()
say str = do
  gs@GlobalState{..} <- get
  liftIO $ do
    putStr $ "Process " ++ show runningId ++ ": "
    putStrLn str

-- | Monad d is a dummy Monad, as we only need the Monadic powers of the State.
-- We don`t need IO or anything else here.
send :: (Eq msg, Show msg, Read msg, Show st, Monad m) =>
        ProcessId -> msg -> VProcess (GlobalState st msg) m ()
send pidTo msg = do
  gs@GlobalState{..} <- get
  let mess = Message runningId pidTo msg
  put $ GlobalState states runningId nextId (mess : sent) received
  return ()

spawn :: (Monad m, Show st, Show msg) =>
         VProcess (GlobalState st msg) m st -> VProcess (GlobalState st msg) m ProcessId
spawn initiateState = do
  gs@GlobalState{..} <- get
  put $ GlobalState states nextId (nextId + 1) sent received
  st <- initiateState
  let newStates = M.insert nextId st states
  put $ GlobalState newStates runningId (nextId + 1) sent received
  return nextId

-- | A VProcess can call this function to terminate.
terminate :: (Monad m,Show st, Show msg) => VProcess (GlobalState st msg) m ()
terminate = do
  gs@GlobalState{..} <- get
  let newStates = M.delete runningId states
  put $ GlobalState newStates runningId (nextId + 1) sent received

-- | A VProcess can call this function to take its state.
myState :: (Monad m,Show st, Show msg) => VProcess (GlobalState st msg) m st
myState = do
  gs@GlobalState{..} <- get
  return $ fromJust $ M.lookup runningId states

-- | A VProcess can call this function to take its state.
insertState :: (Monad m,Show st, Show msg) => st -> VProcess (GlobalState st msg) m ()
insertState st = do
  gs@GlobalState{..} <- get
  let newStates = M.insert runningId st states
  put $ GlobalState newStates runningId nextId sent received

-- | A VProcess can call this function to transform its state.
applyS :: (Monad m,Show st, Show msg) => (st -> st) -> VProcess (GlobalState st msg) m st
applyS transf = do
  gs@GlobalState{..} <- get
  st <- myState
  let newMyState = transf st
  insertState newMyState
  return newMyState
