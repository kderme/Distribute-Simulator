{-# LANGUAGE RecordWildCards #-}
module Schedulers.Interactive where

import Data.List as L
import Control.Monad.State
import VProcess

interactive :: (Eq msg, Show msg, Show st, Read msg) => VP st IO msg (ProcessId,msg)
interactive = do
  gs@GlobalState{..} <- get
  let s = 0 :: Int
  _ <- liftIO $ do
              putStrLn ""
              putStrLn "--------------------------------------"
              putStrLn "Interractive simulator!!"
              print gs
              putStrLn "Pick a sent message to deliver:"
              mapM (\ (n,mess) -> putStrLn $ " " ++ show n ++ ". " ++ show mess) (zip [s..] sent)
  ks <- liftIO getLine
  let k = read ks :: Int
      (mess@(Message _ to msg),newSent) = deleteL k sent
      newReceived = mess : received
  liftIO $ putStrLn "--------------------------------------"
  put $ GlobalState states functions runningId nextId newSent newReceived
  return (to,msg)

deleteL :: Eq a => Int -> [a] -> (a,[a])
deleteL k ls =
  let
    a = ls !! k
  in
    (a, L.delete a ls)
