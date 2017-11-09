{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFoldable #-}

module TreeSimulator where

import Control.Monad.State
import VProcess
import Tree as T
import Data.List as L
import Data.Map as M

simulator :: (Show st, Show msg, Read msg, Eq msg) =>
             VP st msg mem (Maybe (ProcessId,msg)) -> VP st msg mem (T.Tree (Conf st msg) (Message msg))
simulator _ = dfs

-- | The dfs traversal of the the Configuration Tree.
-- The tree is traversed while being created.
dfs :: (Show st, Show msg, Read msg, Eq msg) => VP st msg mem (T.Tree (Conf st msg) (Message msg))
dfs = do
  conf@Configuration{..} <- get
  newTree <- mapM dosth sent
  when (L.null newTree) $ liftIO $ print states
  -- Once we are done with this node and all its children, the whole configuration
  -- is useless. We only store the Conf for the statistics.
  return $ T.Tree (toConf conf) newTree
    where
      dosth :: (Read msg, Show st, Show msg, Eq msg) =>
               Message msg -> VP st msg mem (Message msg,T.Tree (Conf st msg) (Message msg))
      dosth mess@(Message _ to msg) = do
        conf@Configuration{..} <- get
        let sentDeleted = L.delete mess sent
      -- this is extremely buggy prone. sentDeleted is the sent list after deleting the message.
        modify (\ (Configuration a b _ c _ e f) -> Configuration a b to c sentDeleted e f)
        fun <- myFunction
        fun msg
        modify (\ (Configuration a b _ c d e f) -> Configuration a b 0 c d e f)
        tr <- dfs
        put conf
        return (mess,tr)

runSimulation :: (Show st, Show msg, Stateable mem) =>
             VP st msg mem v -> VP st msg mem () -> IO v
runSimulation sim master = do
  let st = do
              master
              sim
  (v, _) <- runStateT st initState
  return v

data Conf st msg = Conf {
    cstates    :: [(ProcessId, st)]
  , csent      :: [Message msg]
}

toConf :: (Show st, Show msg) => Configuration st msg mem -> Conf st msg
toConf Configuration{..} = Conf (M.toList states) sent

-- | Dummy.
treeScheduler :: VP st msg mem (Maybe (ProcessId,msg))
treeScheduler = return Nothing

checkingTree :: (Show msg, Show st,Eq msg, Read msg) => T.Tree (Conf st msg) (Message msg)-> IO ()
checkingTree tree = do
  let x = fmap (show . cstates) tree
  print x
  let statesNum = length $ T.flatten x
  let leavesNum = countLeaves x
  putStrLn $ "Number of Nodes : " ++ show statesNum
  putStrLn $ "Number of Leaves: " ++ show leavesNum

{-}
-- | Catamorphism on trees.
foldTree :: (a -> [b] -> b) -> Tree a b -> b
foldTree f = go where
    go (Tree x ts) = f x (L.map go ts)

mySum :: Num b => [b] -> b
mySum [] = 1
mySum ls = L.sum ls
-}
