{-# LANGUAGE QuasiQuotes #-}
module Cli where

import Debug.Trace


import Data.String.Interpolate (i)

import Data.Either
import Data.List
import Data.Maybe
import System.Console.GetOpt

data Config = MkConfig
  { iterations :: Int
  , population :: Int
  , target :: String
  , verbose :: Bool
  }

data Flag
  = Iterations Int
  | Population Int
  | Target String
  | Verbose
  | Help
  deriving Show

instance Eq Flag where
  Iterations x == Iterations y = x == y
  Population x == Population y = x == y
  Target x == Target y = x == y
  Verbose == Verbose = True
  Help == Help = True
  _ == _ = False

defaultConfig :: Config
defaultConfig = MkConfig 0 0 "" False

parseIterations :: String -> Either String Flag
parseIterations "" = Left "Invalide value for --iterations"
parseIterations str =
  Right $ Iterations $ read str

parsePopulation :: String -> Either String Flag
parsePopulation "" = Left "Invalide value for --population"
parsePopulation str =
  Right $ Population $ read str

flagSpecs :: [OptDescr Flag]
flagSpecs =
  [ Option ['v'] ["verbose"] (NoArg Verbose) "Enable verbose output."
  , Option ['h'] ["help"] (NoArg Help) "Show this help."
  , Option ['i'] ["iterations"] (ReqArg (fromRight (error "incorrect value") . parseIterations) "N") "How many iterations to run."
  , Option ['p'] ["population"] (ReqArg (fromRight (error "incorrect value") . parsePopulation) "P") "How large the population is."
  , Option ['t'] ["target"] (ReqArg Target "T") "How large the population is."
  ]

usage :: String
usage = usageInfo "Usage: keylay-gen [OPTIONS]" flagSpecs

handleNonOptions :: [String] -> [String]
handleNonOptions [] = []
-- handleNonOptions [_] = []
handleNonOptions args = [[i|unexpected args: #{unwords args}|]]

applyFlag :: Flag -> Config -> Config
applyFlag (Iterations n) cfg = cfg { iterations = n }
applyFlag (Population p) cfg = cfg { population = p }
applyFlag (Target str) cfg = cfg { target = str }
applyFlag Verbose cfg = cfg { verbose = True }
applyFlag Help cfg = cfg

wantsHelp :: [Flag] -> Bool
wantsHelp = isJust . Data.List.find (== Help)

skimProgPath :: [String] -> [String]
skimProgPath [] = []
skimProgPath (_ : xs) = xs

parseArgs :: [String] -> Either String Config
parseArgs strs =
  let (options, nonOptions, unrecognized, errors) = traceWith show $ getOpt' RequireOrder flagSpecs $ {- skimProgPath -} strs
      errs =
        errors
        ++ map (\u -> [i|unrecognized option #{u}|]) unrecognized
        ++ handleNonOptions nonOptions
  in case errs of
    (_ : _) -> Left $ unlines (errs ++ ["", usage])
    [] ->
      if wantsHelp options
      then Left usage
      else Right (foldl (flip applyFlag) defaultConfig options)
