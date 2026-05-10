-- | Chromosome representation and genetic operators for keyboard-layout evolution.
--
-- === Conceptual Model
--
-- A keyboard layout is represented as a rearrangement of a base character map:
--
-- > @solutionMap = "abcdefghijklmnopqrstuvwxyz**"@
--
-- (The two trailing @*@ are padding to reach length 30.)
--
-- Each individual in the GA population is a 'Guide' — a list of 28 swap-pair
-- specifications. Each swap pair is @(indexA, Dominance indexB)@ and means
-- "swap the characters at positions @indexA@ and @indexB@ in the base map."
-- The 28 pairs are applied sequentially to produce the final layout string.
--
-- === Genetic Operators
--
-- * 'newGuide' — Generate a random individual: 28 random index pairs.
-- * 'crossover' — Single-point crossover on the chromosome (list of pairs).
-- * 'mutate'    — Replace one random pair in the chromosome with a fresh random pair.
--
-- === Solution Decoding
--
-- 'solution' takes a 'Guide' and applies all its swap pairs to the base map
-- to produce the actual layout string, which is then scored against the
-- target using Levenshtein distance (in 'Main').
module MapGuide where

import Control.Monad
import Data.Bifunctor
import Data.List
import System.Random
import qualified Data.Vector as Vector

-- | The base character map that gets rearranged by swap operations.
--   26 lowercase letters + 2 padding chars = 30 total.
--
-- > total
-- > doubleMult : (n : Nat) -> 2 * n = n + n
-- > doubleMult n = rewrite plusCommutative n 0 in Refl
data Map = MkMap [Char]

-- | A dominance value attached to each swap-pair index. Currently unused
--   beyond being stored; always initialized to 0 and set to 1 on mutation.
data Dominance = MkDominance Int

-- | A chromosome (individual) in the GA population.
--
-- Encoded as a vector of @(position, Dominance)@ pairs. The vector has length
-- @2 * guideSize@ (= 56), interpreted as 28 consecutive pairs of indices.
-- Each pair @(i, j)@ means "swap characters at position i and j in the map."
data Guide = MkGuide (Vector.Vector (Int, Dominance))

-- | Number of swap pairs per chromosome (28 pairs = 56 flat elements).
--   Chosen so the chromosome has enough degrees of freedom to explore
--   the space of permutations of the 30-character base map.
guideSize :: Int
guideSize = 28

-- | Wrapper type for a solution (currently only holds a left Guide;
--   appears to be a leftover from a symmetric left\/right design).
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

-- | Generate a random 'Guide' individual.
--
-- Creates @2 * guideSize@ (= 56) flat elements, each a random index in
-- @[0, guideSize)@ paired with @Dominance 0@. These are later grouped
-- into 28 swap pairs by 'pairings'.
newGuide :: IO Guide
newGuide =
  MkGuide <$> Vector.replicateM (2 * guideSize) ((, MkDominance 0) <$> rndIndex guideSize)

-- | The base character map: lowercase alphabet + 2 padding characters.
--   A 'Guide' rearranges this map via its swap pairs to produce a layout.
solutionMap :: Map
solutionMap = MkMap "abcdefghijklmnopqrstuvwxyz**"

-- | Split a list into @n@-sized chunks, then transpose so each chunk
--   becomes a column. Used to reshape the flat chromosome into pairs.
--
-- Example: @nSplits 2 3 "abcdef" = ["ac","bd","ce","df"]@
nSplits :: Int -> Int -> [a] -> [[a]]
nSplits k n = transpose . kSplits k n

-- | Split a list into @k@ chunks of size @n@.
kSplits :: Int -> Int -> [a] -> [[a]]
kSplits 0 _ _ = []
kSplits k n xs =
  let (ys, zs) = splitAt n xs
  in ys : kSplits (k - 1) n zs

-- | Group the flat chromosome (56 elements) into 28 consecutive pairs.
--
-- Each pair @(idxA, idxB)@ represents one swap operation to apply to
-- the base map. The pairs are applied left-to-right in 'solution'.
pairings :: Vector.Vector (Int, Dominance) -> Vector.Vector ((Int, Dominance), (Int, Dominance))
pairings xs = Vector.zip (Vector.take (Vector.length xs `div` 2) xs) (Vector.drop (Vector.length xs `div` 2) xs)

-- | Replace the element at a given index in a list.
replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 y (_:xs) = y:xs
replaceAt n y (x:xs) = x : replaceAt (n - 1) y xs

-- | Swap two elements in a list at the given indices.
swap :: (Int, Int) -> [a] -> [a]
swap (x, y) xs = let
  a = xs !! x
  b = xs !! y
  in replaceAt x b $ replaceAt y a xs

-- | Apply a list of swap operations sequentially to a list.
updateMap :: [(Int, Int)] -> [a] -> [a]
updateMap xs ys = foldl (flip swap) ys xs

-- | Extract just the indices from a pair of @(index, Dominance)@ values,
--   discarding the dominance metadata.
extractSwap :: ((a, d), (b, c)) -> (a, b)
extractSwap ((a, _), (b, _)) = (a, b)

-- | Decode a 'Guide' chromosome into an actual layout string.
--
-- Process:
-- 1. Group the flat chromosome into swap pairs via 'pairings'.
-- 2. Extract the raw indices, discarding dominance values.
-- 3. Apply all swaps sequentially to the base map.
solution :: Map -> Guide -> String
solution (MkMap xs) (MkGuide guide) =
  updateMap (map extractSwap $ Vector.toList $ pairings guide) xs

-- | Generate a random index in @[0, n)@.
rndIndex :: Int -> IO Int
rndIndex n = randomRIO (0, n - 1)

-- | Mutate a 'Guide' by replacing one random element in its flat
--   chromosome with a fresh random @(index, Dominance 1)@ pair.
mutate :: Guide -> IO Guide
mutate (MkGuide xs) = do
  target <- rndIndex (2 * guideSize)  -- Position to mutate in flat vector.
  value  <- rndIndex guideSize        -- New random index value.
  let updated = xs Vector.// [(target, (value, MkDominance 1))]
  pure $ MkGuide updated

-- | Single-point crossover on two chromosomes.
--
-- Picks a random cut point and swaps the tails of the two parent
-- chromosomes, producing two offspring. This is a standard 1-point
-- crossover adapted for the flat-vector representation.
crossover :: Guide -> Guide -> IO (Guide, Guide)
crossover (MkGuide left) (MkGuide right) = do
  target <- rndIndex (2 * guideSize)  -- Random cut point.
  pure $ bimap MkGuide MkGuide $ crossoverAt target left right

-- | Internal helper: apply crossover at a given cut point.
--   When @cut@ reaches 0, the remaining tails are swapped.
crossoverAt ::
  Int -> -- ^ Cut point.
  Vector.Vector a -> -- ^ Left parent chromosome (flat vector).
  Vector.Vector a -> -- ^ Right parent chromosome (flat vector).
  (Vector.Vector a, Vector.Vector a) -- ^ Two offspring chromosomes.
crossoverAt cut left right =
  let (left1, left2) = Vector.splitAt cut left
      (right1, right2) = Vector.splitAt cut right
  in (Vector.concat [left1, right2], Vector.concat [right1, left2])
