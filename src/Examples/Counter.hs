{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Examples.Counter where

import           System.Environment
import           VProcess
import           Simulator
import           Schedulers.Interactive
import           Control.Monad.State
import           Data.Binary
import           GHC.Generics
import           Data.Typeable

run :: IO ()
run = do
  [n,m] <- getArgs
  let sim = simulator scedule
  finalState <- runSimulation sim (spawner (read n) (read m))
  return ()

type VProcess v = VP MyState MyMessage () v

type Count = Int

data MyMessage =
      Inc ProcessId
    | Report Count
    | Done
    deriving (Eq, Read, Show, Generic, Typeable, Binary)

data MyState = Counter Int | Consumer Int | Master Int deriving (Show)

spawner :: Int -> Int -> VProcess ()
spawner n m = do
  masterId <- spawn masterInit $ master n
  counterPid <- spawn counterInit counter
  _ <- replicateM_ n $ spawn (consumerInit counterPid) (consumer m counterPid masterId)
  return ()

masterInit :: VProcess MyState
masterInit = do
  say "Hello, I`m the master"
  return $ Master 0

master :: Int -> MyMessage -> VProcess ()
master n Done = do
  say "Got Done"
  Master newState <- applyS addOne
  when (newState == n) $ do
    say "All Done"
    terminate

consumerInit :: ProcessId -> VProcess MyState
consumerInit counterId = do
  say "Hello, I`m a consumer"
  selfPid <- getSelfPid
  send counterId $ Inc selfPid
  return $ Consumer 0

consumer :: Count -> ProcessId -> ProcessId -> MyMessage -> VProcess ()
consumer m counterId masterId (Report n) = do
  say $ "Got Report " ++ show n
  selfId <- getSelfPid
  Consumer newState <- applyS addOne
  if (newState == m)
  then do
    send masterId Done
    terminate
  else send counterId $ Inc selfId

counterInit :: VProcess MyState
counterInit = do
  say "Hello, I`m the counter"
  return $ Counter 0

counter :: MyMessage -> VProcess ()
counter (Inc pid) = do
  Counter newState <- applyS addOne
  send pid $ Report newState

addOne :: MyState -> MyState
addOne (Counter n) = Counter $ n + 1
addOne (Consumer n) = Consumer $ n + 1
addOne (Master n)  = Master $ n + 1
