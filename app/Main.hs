module Main where

import VProcess
import Simulator
import Schedulers.Interactive
import Control.Monad.State
import System.Random

main :: IO ()
main = do
  let sim = simulator (fun 4) interactive --interactive
  finalState <- runSimulation sim (master 2)
  return ()

-- | Master is not a real VProcess. He is responsible for spawning all VProcesses.
master :: Int -> VP MyState IO MyMessage ()
master n = do
  a <- spawn initAgent1
  b <- spawn initAgent1
  ls <- replicateM n $ spawn initAgent2
  send 2 "Hello"
  return ()

initAgent1 :: VP MyState IO MyMessage MyState
initAgent1 = do
  liftIO $ putStrLn "hi"
  return 0

initAgent2 :: VP MyState IO MyMessage MyState
initAgent2 = return 1

fun :: Int -> MyMessage -> VP MyState IO MyMessage ()
fun n msg = do
  say $ "Got " ++ show msg
  applyS (+1)
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
