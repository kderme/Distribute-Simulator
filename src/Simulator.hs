{-# LANGUAGE RecordWildCards #-}

module Simulator where

import Control.Monad.State
import VProcess

simulator :: (Show st, Show msg) =>
             VP st IO msg (ProcessId,msg) -> VP st IO msg ()
simulator scheduler = forever $ do
  (pid,msg) <- scheduler
  modify (\ (GlobalState a b _ c d e) -> GlobalState a b pid c d e)
  fun <- myFunction
  fun msg
  modify (\ (GlobalState a b _ c d e) -> GlobalState a b 0 c d e) -- GlobalState states 0 nextId sent received
  return ()

runSimulation :: (Show st, Show msg) =>
             VP st IO msg () -> VP st IO msg () -> IO (GlobalState st msg)
runSimulation sim master = do
  let st = do
              master
              sim
  execStateT st initState

{-}
spawnVNode :: (Show st, Show msg) =>
                  (VP st IO msg ()) ->
-}
