{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module VProcess where

import Control.Monad.State
import Data.Map as M
import Data.Maybe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type ProcessId = Int

data (Show msg, Read msg, Eq msg) => Message msg = Message {
    mfrom :: ProcessId
  , mto   :: ProcessId
  , mmsg  :: msg
} deriving (Show, Eq)

-- | Class of types that act as states an so they have an initial default state.
class Stateable st where
  initState :: st

data (Show st, Show msg) => Configuration st msg mem = Configuration {
    states    :: M.Map ProcessId st
  , functions :: M.Map ProcessId (msg -> VP st msg mem ())
  , runningId :: ProcessId
  , nextId    :: ProcessId
  , sent      :: [Message msg]
  , received  :: [Message msg]
  , simMem    :: mem
  }

-- | This is a subset of Configuration. Quick and dirty way to instantiate Show.
data (Show st, Show msg) => Configuration' st msg = Configuration' {
    states'    :: M.Map ProcessId st
  , runningId' :: ProcessId
  , nextId'    :: ProcessId
  , sent'      :: [Message msg]
  , received'  :: [Message msg]
  } deriving (Show)

instance (Show st, Show msg, Read msg, Eq msg) => Show (Configuration st msg mem) where
  show Configuration{..} = show $ Configuration' states runningId nextId sent received

type VPT st msg mem m v = StateT (Configuration st msg mem) m v

type VP st msg mem v = StateT (Configuration st msg mem) IO v

instance (Show st, Show msg, Stateable mem) => Stateable (Configuration st msg mem) where
  initState = Configuration M.empty M.empty 0 1 [] [] initState

getSelfPid :: (Monad m, Show st, Show msg) =>
              VPT st msg mem m ProcessId
getSelfPid = do
  Configuration{..} <- get
  return runningId

say :: (Eq msg, Show msg, Read msg, Show st) =>
       String -> VP st msg mem ()
say str = do
  Configuration{..} <- get
  liftIO $ do
    putStr $ "Process " ++ show runningId ++ ": "
    putStrLn str

-- | Monad d is a dummy Monad, as we only need the Monadic powers of the State.
-- We don`t need IO or anything else here.
send :: (Eq msg, Show msg, Read msg, Show st, Monad m) =>
        ProcessId -> msg -> VPT st msg mem m ()
send pidTo msg = do
  Configuration{..} <- get
  let mess = Message runningId pidTo msg
  put $ Configuration states functions runningId nextId (mess : sent) received simMem
  return ()

-- | Spawns a new VProcess.
spawn :: (Monad m, Show st, Show msg) =>
         VPT st msg mem m st ->
         (msg -> VP st msg mem ()) ->
         VPT st msg mem m ProcessId
spawn initiateState fun = do
  Configuration{..} <- get
  modify $ \ (Configuration a b _ _ c d e) -> Configuration a b nextId (nextId + 1) c d e
  st <- initiateState
  let newStates = M.insert nextId st states
  let newFunctions = M.insert nextId fun functions
  modify $ \ (Configuration _ _ _ _ c d e) -> Configuration newStates newFunctions runningId (nextId + 1) c d e
  return nextId

-- | A VProcess can call this function to terminate.
terminate :: (Monad m,Show st, Show msg) => VPT st msg mem m ()
terminate = do
  Configuration{..} <- get
  let newStates = M.delete runningId states
  put $ Configuration newStates functions runningId (nextId + 1) sent received simMem

-- | A VProcess can call this function to take its state.
myState :: (Monad m,Show st, Show msg) => VPT st msg mem m st
myState = do
  Configuration{..} <- get
  return $ fromJust $ M.lookup runningId states

-- | The simulator can call this function to take the function of the VProcess
-- that is about to run.
myFunction :: (Monad m,Show st, Show msg) =>
              VPT st msg mem m (msg -> VP st msg mem ())
myFunction = do
  Configuration{..} <- get
  return $ fromJust $ M.lookup runningId functions

-- | A VProcess can call this function to change its state.
insertState :: (Monad m,Show st, Show msg) => st -> VPT st msg mem m ()
insertState st = do
  Configuration{..} <- get
  let newStates = M.insert runningId st states
  put $ Configuration newStates functions runningId nextId sent received simMem

-- | A VProcess can call this function to transform its state.
applyS :: (Monad m,Show st, Show msg) => (st -> st) -> VPT st msg mem m st
applyS transf = do
  Configuration{..} <- get
  st <- myState
  let newMyState = transf st
  insertState newMyState
  return newMyState
