module Simulator where

import Control.Monad.State
import VProcess

simulator :: (Show st, Show msg) =>
             VP st msg mem (Maybe (ProcessId,msg)) -> VP st msg mem ()
simulator scheduler = go
  where
    go = do
      mdel <- scheduler
      case mdel of
        (Just (pid,msg1)) -> do
          modify (\ (Configuration a b _ c d e f) -> Configuration a b pid c d e f)
          fun <- myFunction
          fun msg1
          modify (\ (Configuration a b _ c d e f) -> Configuration a b 0 c d e f)
          go
        Nothing -> return ()

-- | This turns a VProcess to an IO. master will be replaced by spawn master,
-- when bugs are fixed, so that the first VProcess is not a special kind of VProcess.
runSimulation :: (Show st, Show msg, Stateable mem) =>
             VP st msg mem () -> VP st msg mem () -> IO (Configuration st msg mem)
runSimulation sim master = do
  let st = do
              master
              sim
  execStateT st initState

{-}
spawnVNode :: (Show st, Show msg) =>
                  (VP st IO msg ()) ->
-}
