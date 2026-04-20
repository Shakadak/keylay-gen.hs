module GenAlgo where

import Control.Monad
import Data.List

iterPop :: Monad m => Eq val => Show val
  => (List val -> m (List val)) -- rank
  -> (val -> val -> m (List val)) -- combine
  -> (List val -> m ()) -- inspect
  -> List val
  -> m (List val)
iterPop rank combine inspect oldGen = do
  nextGen <- Control.Monad.zipWithM combine oldGen (drop 1 oldGen)
  pop <- rank $ nub (oldGen ++ concat nextGen)
  let pop' = take (length oldGen) pop
  inspect pop'
  pure pop'
