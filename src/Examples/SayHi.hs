module Examples.SayHi where

import VProcess
import Simulator
import Schedulers.Interactive
import Control.Monad.State
import System.Random

run :: IO ()
run = do
  let sim = simulator scedule --interactive
  finalState <- runSimulation sim (master 2)
  return ()

type VProcess v = VP MyState MyMessage () v

-- | Master is not a real VProcess. He is responsible for spawning all VProcesses.
master :: Int -> VProcess ()
master n = do
  _ <- spawn initAgent1 (fun 4)
  _ <- spawn initAgent1 (fun 4)
  replicateM_ n $ spawn initAgent2 (fun 4)
  send 2 "Hello"
  return ()

initAgent1 :: VProcess MyState
initAgent1 = return 0

initAgent2 :: VProcess MyState
initAgent2 = return 1

fun :: Int -> MyMessage -> VProcess ()
fun n msg = do
  say $ "Got " ++ show msg
  _ <- applyS (+1)
  x <- liftIO $ randomRIO (1, n)
  send x "hi"
  liftIO $ print x
  y <- liftIO $ randomRIO (1, n)
  send y "hi"
  liftIO $ print y
  return ()
--  send

type MyState = Int

type MyMessage = String
