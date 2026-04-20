module MapGuide where

import Control.Monad
import Data.Bifunctor
import Data.List
import System.Random


{- total
doubleMult : (n : Nat) -> 2 * n = n + n
doubleMult n = rewrite plusCommutative n 0 in Refl -}

data Map = MkMap [Char]

data Dominance = MkDominance Int

data Guide = MkGuide [(Int, Dominance)]

guideSize :: Int
guideSize = 28

data Solution = MkSolution
  { left :: Guide
  }

instance Show Dominance where
  show (MkDominance n) = show n

instance Eq Dominance where
  (MkDominance x) == (MkDominance y) = x == y

instance Show Guide where
  show (MkGuide xs) = show xs

instance Eq Guide where
  (MkGuide xs) == (MkGuide ys) = xs == ys

newGuide :: IO Guide
newGuide =
  MkGuide <$> Control.Monad.replicateM (2 * guideSize) ((, MkDominance 0) <$> rndIndex guideSize)

solutionMap :: Map
solutionMap = MkMap "abcdefghijklmnopqrstuvwxyz**"

nSplits :: Int -> Int -> [a] -> [[a]]
nSplits k n = transpose . kSplits k n

kSplits :: Int -> Int -> [a] -> [[a]]
kSplits 0 _ _ = []
kSplits k n xs =
  let (ys, zs) = splitAt n xs
  in ys : kSplits (k - 1) n zs

pairings :: [a] -> [(a, a)]
pairings xs = map (\[a, b] -> (a, b)) $ nSplits 2 guideSize xs

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 y (_:xs) = y:xs
replaceAt n y (x:xs) = x : replaceAt (n - 1) y xs

swap :: (Int, Int) -> [a] -> [a]
swap (x, y) xs = let
  a = xs !! x
  b = xs !! y
  in replaceAt x b $ replaceAt y a xs

updateMap :: [(Int, Int)] -> [a] -> [a]
updateMap xs ys = foldl (flip swap) ys xs

extractSwap :: ((a, d), (b, c)) -> (a, b)
extractSwap ((a, _), (b, _)) = (a, b)

solution :: Map -> Guide -> String
solution (MkMap xs) (MkGuide guide) =
  updateMap (map extractSwap $ pairings guide) xs

rndIndex :: Int -> IO Int
rndIndex n = randomRIO (0, n - 1)

mutate :: Guide -> IO Guide
mutate (MkGuide xs) = do
  target <- rndIndex (2 * guideSize)
  value <- rndIndex guideSize
  pure $ MkGuide $ replaceAt target (value, MkDominance 1) xs

crossoverAt ::
  Int -> -- cut
  [a] ->
  [a] ->
  ([a], [a])
crossoverAt 0 left right = (right, left)
crossoverAt cut (l : ls) (r : rs) =
  let (left, right) = crossoverAt (cut - 1) ls rs
  in (l : left, r : right)
crossoverAt _ _ _ = error "Mismatched crossover chromosomes"

crossover :: Guide -> Guide -> IO (Guide, Guide)
crossover (MkGuide left) (MkGuide right) = do
  target <- rndIndex (2 * guideSize)
  pure $ bimap MkGuide MkGuide $ crossoverAt target left right
