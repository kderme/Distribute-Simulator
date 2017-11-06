{-# LANGUAGE RecordWildCards #-}

module Schedulers.FifoScheduler where

import qualified Data.List as L
import           Control.Monad.State
import           VProcess
import qualified Data.Tree as T
import qualified Data.Map as M

data Conf st msg = Conf {
    cstates    :: M.Map ProcessId st
  , csent      :: [Message msg]
}

toConf :: (Show st, Show msg) => Configuration st msg mem -> Conf st msg
toConf Configuration{..} = Conf states sent

-- | It`s time for the configuration Tree.
data MyMem st msg = MyMem {
    conf :: T.Tree (Conf st msg)
  , nextIdex :: Int
}
