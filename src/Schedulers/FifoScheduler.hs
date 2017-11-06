{-# LANGUAGE RecordWildCards #-}

module Schedulers.FifoScheduler where

import qualified Data.List as L
import           Control.Monad.State
import           VProcess

-- | The Fifo scheduler needs no memory.
type MyMemory = ()

instance Stateable () where
  initState = ()

scedule :: (Eq msg, Show msg, Show st, Read msg) => VP st msg () (Maybe (ProcessId,msg))
scedule = do
    gs@Configuration{..} <- get
    let mdel = delete sent
    case mdel of
      Nothing -> return Nothing
      Just (mess@(Message _ to msg),newSent) -> do
        let newReceived = mess : received
        put $ Configuration states functions runningId nextId newSent newReceived simMem
        return $ Just (to,msg)

delete :: Eq a => [a] -> Maybe (a,[a])
delete [] = Nothing
delete ls =
  let
    (r : rev) = L.reverse ls
  in
    Just (r, reverse rev)
