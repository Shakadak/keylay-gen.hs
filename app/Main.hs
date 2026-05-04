{-# LANGUAGE QuasiQuotes #-}
module Main where

import Debug.Trace

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

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ x = x
nTimes 1 f x = f x
nTimes n f x = nTimes (n - 1) f (f x)

-- selectSplit :: [a] -> (a, [a])
-- selectSplit [] = ?selectSplit_rhs_0
-- selectSplit (x :: xs) = ?selectSplit_rhs_1

genePool :: [Char]
genePool = "abcdefghijklmnopqrstuvwxyz"

rndSelect :: [a] -> IO a
rndSelect xs = (xs !!) <$> randomRIO (0, length xs - 1)

randGene :: IO Char
randGene = rndSelect genePool

genMember :: Int -> IO String
genMember n = replicateM n randGene


genPop' :: Int -> Int -> IO [String]
genPop' n s = replicateM n $ genMember s

sortByM :: Monad m => Ord o => (a -> m o) -> [a] -> m [a]
sortByM by xs = do
  ts <- traverse (\x -> (x,) <$> by x) xs
  pure $ map fst $ sortBy (compare `on` snd) ts

combine' :: String -> String -> IO [String]
combine' l r = do
  let max' = length l - 1
  needle <- randomRIO (0, max')
  let (hl, tl) = splitAt needle l
  let (hr, tr) = splitAt needle r
  pure [hl ++ tr, hr ++ tl]

mutate :: String -> IO String
mutate str = do
  let max' = length str - 1
  needle <- randomRIO (0, max')
  char <- randGene
  let unpkd = str
      new = case needle < length str of
        True -> replaceAt needle char unpkd
        False -> unpkd
  pure new


program :: IO ()
program = do
  args <- getArgs
  case parseArgs $ traceWith show args of
    Left error' -> putStr error'
    Right cfg -> do
      putStrLn [__i|
      target = #{target cfg}
      iterations = #{show $ iterations cfg}
      population = #{show $ population cfg}
      verbose = #{show $ verbose cfg}
      |]
      let
        eval :: Guide -> Int
        eval = levenshtein (pack $ target cfg) . pack . solution solutionMap

        rank :: [Guide] -> [Guide]
        rank = sortOn eval

        genPop :: Int -> IO [Guide]
        genPop n = replicateM n newGuide

        format :: (Guide, Int) -> String
        format (guide, score) =
          [i|#{solution solutionMap guide} => #{show score}|]

        combine :: Guide -> Guide -> IO [Guide]
        combine left right = do
          chance <- randomRIO (0.0, 1.0) :: IO Double
          (left', right') <- if chance > 0.1 then crossover left right else pure (left, right)
          chance' <- randomRIO (0.0, 1.0) :: IO Double
          left'' <- if chance' > 0.1 then MapGuide.mutate left' else pure left'
          chance'' <- randomRIO (0.0, 1.0) :: IO Double
          right'' <- if chance'' > 0.1 then MapGuide.mutate right' else pure right'
          pure [left'', right'']

        inspect :: [Guide] -> IO ()
        inspect pop = do
          let formatted = maybe "" (format . (\x -> (x, eval x))) $ listToMaybe pop
          putStrLn [i|Target: #{target cfg}; Intermediate top member: #{formatted}|]

        iterator :: IO [Guide] -> IO [Guide]
        iterator = (>>= iterPop (pure . rank) combine inspect)

      initPop <- rank <$> genPop (population cfg)
      let ts = map (\x -> (x, eval x)) initPop
      putStrLn [i|Initial population:      #{unwords $ map format ts}|]
      finalPop <- rank `fmap` nTimes (iterations cfg) iterator (pure initPop)
      let ts' = map (\x -> (x,) $ eval x) finalPop
      putStrLn [i|Final population:        #{unwords $ map format ts'}|]

main :: IO ()
main = program
