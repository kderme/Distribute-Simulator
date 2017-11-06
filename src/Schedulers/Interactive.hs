{-# LANGUAGE RecordWildCards #-}
module Schedulers.Interactive where

import qualified Data.List as L
import           Control.Monad.State
import           VProcess

-- | The interactive scheduler needs no memory.
type MyMemory = ()

instance Stateable () where
  initState = ()

scedule :: (Eq msg, Show msg, Show st, Read msg) => VP st msg () (Maybe (ProcessId,msg))
scedule = do
  gs@Configuration{..} <- get
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
      mdel = delete k sent
  case mdel of
    Nothing -> return Nothing
    Just (mess@(Message _ to msg),newSent) -> do
      let newReceived = mess : received
      liftIO $ putStrLn "--------------------------------------"
      put $ Configuration states functions runningId nextId newSent newReceived simMem
      return $ Just (to,msg)

delete :: Eq a => Int -> [a] -> Maybe (a,[a])
delete k ls = do
    a <- nth k ls
    return (a, L.delete a ls)

nth :: Int -> [a] -> Maybe a
nth _ []       = Nothing
nth 0 (x : _)  = Just x
nth n (_ : xs) = nth (n - 1) xs
