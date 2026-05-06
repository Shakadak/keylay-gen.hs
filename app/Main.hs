{-# LANGUAGE QuasiQuotes #-}

-- | Entry point: wire together CLI, GA loop, and layout evolution.
--
-- === High-Level Flow
--
-- 1. Parse CLI arguments to get target string, population size, and iteration count.
-- 2. Generate a random initial population of 'Guide' chromosomes.
-- 3. Rank each individual by fitness: Levenshtein distance from the
--    decoded layout ('solution') to the target string.
-- 4. Run @N@ generations of crossover + mutation via 'iterPop'.
-- 5. Print the final population's best layouts and scores.
--
-- === Fitness Function
--
-- Fitness = Levenshtein distance between the decoded layout string and
-- the target. Lower distance = better fit. The population is sorted
-- ascending, so the best individual is always first.
module Main where

import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Text (pack)
import System.Environment (getArgs)
import System.Random
import Data.Text.Metrics (levenshtein)
import Data.String.Interpolate (i, __i)

import Cli
import GenAlgo
import MapGuide

-- | Apply a function @f@ exactly @n@ times to a value.
--   Used to iterate the GA loop for the configured number of generations.
nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ x = x
nTimes 1 f x = f x
nTimes n f x = nTimes (n - 1) f (f x)

-- selectSplit :: [a] -> (a, [a])
-- selectSplit [] = ?selectSplit_rhs_0
-- selectSplit (x :: xs) = ?selectSplit_rhs_1

-- | Character pool for random gene generation (unused in current Guide-based design;
--   leftover from an earlier string-based chromosome representation).
genePool :: [Char]
genePool = "abcdefghijklmnopqrstuvwxyz"

-- | Randomly select an element from a list.
rndSelect :: [a] -> IO a
rndSelect xs = (xs !!) <$> randomRIO (0, length xs - 1)

-- | Generate a random character from 'genePool'.
randGene :: IO Char
randGene = rndSelect genePool

-- | Generate a random string of length @n@ from 'genePool'.
genMember :: Int -> IO String
genMember n = replicateM n randGene

-- | Generate a population of @n@ random strings of length @s@.
--   (Unused in current design; leftover from earlier string-based representation.)
genPop' :: Int -> Int -> IO [String]
genPop' n s = replicateM n $ genMember s

-- | Sort a list by a monadic key function.
sortByM :: Monad m => Ord o => (a -> m o) -> [a] -> m [a]
sortByM by xs = do
  ts <- traverse (\x -> (x,) <$> by x) xs
  pure $ map fst $ sortBy (compare `on` snd) ts

-- | Crossover two strings at a random split point (unused; leftover).
combine' :: String -> String -> IO [String]
combine' l r = do
  let max' = length l - 1
  needle <- randomRIO (0, max')
  let (hl, tl) = splitAt needle l
  let (hr, tr) = splitAt needle r
  pure [hl ++ tr, hr ++ tl]

-- | Mutate a string by replacing one random character (unused; leftover).
mutate :: String -> IO String
mutate str = do
  let max' = length str - 1
  needle <- randomRIO (0, max')
  char <- randGene
  let unpkd = str
      new = case needle < length str of
        True  -> replaceAt needle char unpkd
        False -> unpkd
  pure new

-- | Main program: parse args, run GA, report results.
program :: IO ()
program = do
  args <- getArgs
  case parseArgs args of
    Left error' -> putStr error'
    Right cfg -> do
      let targetText = pack $ target cfg
      putStrLn [__i|
      target = #{target cfg}
      iterations = #{show $ iterations cfg}
      population = #{show $ population cfg}
      verbose = #{show $ verbose cfg}
      |]
      let
        -- | Fitness function: Levenshtein distance from decoded layout to target.
        --   Lower = better. Used to rank the population each generation.
        eval :: Guide -> Int
        eval = levenshtein targetText . pack . solution solutionMap

        -- | Rank population by fitness (ascending = best first).
        rank :: [Guide] -> [Guide]
        rank = sortOn eval

        -- | Generate a random initial population of @n@ individuals.
        genPop :: Int -> IO [Guide]
        genPop n = replicateM n newGuide

        -- | Format an individual as "layout_string => levenshtein_score".
        format :: (Guide, Int) -> String
        format (guide, score) =
          [i|#{solution solutionMap guide} => #{show score}|]

        -- | Crossover two 'Guide' individuals with 90% crossover probability,
        --   then independently mutate each offspring with 90% mutation probability.
        combine :: Guide -> Guide -> IO [Guide]
        combine left right = do
          chance  <- randomRIO (0.0, 1.0) :: IO Double
          (left', right') <- if chance > 0.1 then crossover left right else pure (left, right)
          chance'  <- randomRIO (0.0, 1.0) :: IO Double
          left''   <- if chance' > 0.1  then MapGuide.mutate left'  else pure left'
          chance'' <- randomRIO (0.0, 1.0) :: IO Double
          right''  <- if chance'' > 0.1 then MapGuide.mutate right' else pure right'
          pure [left'', right'']

        -- | Print the best individual in the current population for progress tracking.
        inspect :: [Guide] -> IO ()
        inspect pop = do
          let formatted = maybe "" (format . (\x -> (x, eval x))) $ listToMaybe pop
          putStrLn [i|Target: #{target cfg}; Intermediate top member: #{formatted}|]

        -- | One GA generation: rank, crossover/mutate, truncate, inspect.
        --   Takes an IO action producing the current population, returns next population.
        iterator :: IO [Guide] -> IO [Guide]
        iterator = (>>= iterPop (pure . rank) combine inspect)

      -- Generate and rank the initial random population.
      initPop <- rank <$> genPop (population cfg)
      let ts = map (\x -> (x, eval x)) initPop
      putStrLn [i|Initial population:      #{unwords $ map format ts}|]

      -- Run the GA for the configured number of iterations.
      finalPop <- rank `fmap` nTimes (iterations cfg) iterator (pure initPop)
      let ts' = map (\x -> (x,) $ eval x) finalPop
      putStrLn [i|Final population:        #{unwords $ map format ts'}|]

-- | Program entry point.
main :: IO ()
main = program
