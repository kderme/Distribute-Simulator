{-# LANGUAGE RecordWildCards #-}

module Simulator where

import Control.Monad.State
import VProcess

simulator :: (Show st, Show msg) =>
             (msg -> VP st IO msg ()) -> VP st IO msg (ProcessId,msg) -> VP st IO msg ()
simulator agentFun scheduler = forever $ do
  (pid,msg) <- scheduler
  modify (\ (GlobalState a _ c d e) -> GlobalState a pid c d e)
  agentFun msg
  modify (\ (GlobalState a _ c d e) -> GlobalState a 0 c d e) -- GlobalState states 0 nextId sent received
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
