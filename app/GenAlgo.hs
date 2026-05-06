-- | Core genetic-algorithm iteration step.
--
-- This module defines the single generic GA generation loop ('iterPop')
-- used by the keyboard-layout evolution. Given a ranked population, it:
--
-- 1. Pairs adjacent individuals and applies crossover + mutation ('combine').
-- 2. Merges parents and offspring, deduplicates, and re-ranks by fitness.
-- 3. Truncates back to the original population size (elitist selection).
-- 4. Inspects the new generation for progress reporting.
--
-- The algorithm is a simple steady-state GA with tournament-style pairing
-- (each individual mates with its neighbor in the ranked list).
module GenAlgo where

import Control.Monad
import Data.List

-- | Run one generation of the genetic algorithm.
--
-- Parameters:
--
-- * @rank@    — Re-rank a population by fitness (best first).
-- * @combine@ — Crossover two parents, optionally mutating, producing offspring.
-- * @inspect@ — Side-effect hook to report the current best individual.
-- * @oldGen@  — The parent generation (already ranked best-first).
--
-- Returns the next generation, truncated to the same size as @oldGen@.
iterPop :: Monad m => Eq val
  => ([val] -> m [val])                  -- ^ Rank population by fitness (best first).
  -> (val -> val -> m [val])             -- ^ Crossover + mutate two parents into offspring.
  -> ([val] -> m ())                     -- ^ Inspect / report the new generation.
  -> [val]                               -- ^ Current (ranked) parent generation.
  -> m [val]                             -- ^ Next generation (ranked, same size).
iterPop rank combine inspect oldGen = do
  -- Pair each individual with its neighbor: (0,1), (1,2), (2,3), …
  -- This biases mating toward fit individuals since the list is ranked.
  nextGen <- Control.Monad.zipWithM combine oldGen (drop 1 oldGen)

  -- Merge parents + offspring, deduplicate, and re-rank by fitness.
  pop <- rank $ nub (oldGen ++ concat nextGen)

  -- Truncate to original population size (elitist truncation selection).
  let pop' = take (length oldGen) pop

  -- Report progress for this generation.
  inspect pop'
  pure pop'
