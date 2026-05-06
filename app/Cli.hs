{-# LANGUAGE QuasiQuotes #-}

-- | Command-line argument parsing and configuration.
--
-- Provides a simple CLI interface using 'System.Console.GetOpt' to configure
-- the genetic algorithm run. The user specifies:
--
-- * @--target@     : The desired keyboard layout string to evolve toward.
-- * @--iterations@ : How many GA generations to run.
-- * @--population@ : The size of each generation.
-- * @--verbose@    : Enable verbose output.
module Cli where

import Data.String.Interpolate (i)

import Data.Either
import Data.List
import Data.Maybe
import System.Console.GetOpt

-- | Runtime configuration for a GA run.
data Config = MkConfig
  { iterations :: Int    -- ^ Number of generations to evolve.
  , population :: Int    -- ^ Number of individuals per generation.
  , target     :: String -- ^ Target keyboard layout string to approximate.
  , verbose    :: Bool   -- ^ Whether to print verbose progress output.
  }

-- | Supported command-line flags.
data Flag
  = Iterations Int      -- ^ Set number of iterations (@-i@ / @--iterations@).
  | Population Int      -- ^ Set population size (@-p@ / @--population@).
  | Target String       -- ^ Set target string (@-t@ / @--target@).
  | Verbose             -- ^ Enable verbose mode (@-v@ / @--verbose@).
  | Help                -- ^ Show usage and exit (@-h@ / @--help@).
  deriving Show

-- | Structural equality for flags (pattern-match style, not derived,
--   to avoid deriving on constructors with different shapes).
instance Eq Flag where
  Iterations x == Iterations y = x == y
  Population x == Population y = x == y
  Target x     == Target y     = x == y
  Verbose      == Verbose      = True
  Help         == Help         = True
  _            == _            = False

-- | Starting configuration with all fields zeroed/empty.
--   Flags are folded over this to produce the final config.
defaultConfig :: Config
defaultConfig = MkConfig 0 0 "" False

-- | Parse the argument string for @--iterations@.
parseIterations :: String -> Either String Flag
parseIterations "" = Left "Invalide value for --iterations"
parseIterations str =
  Right $ Iterations $ read str

-- | Parse the argument string for @--population@.
parsePopulation :: String -> Either String Flag
parsePopulation "" = Left "Invalide value for --population"
parsePopulation str =
  Right $ Population $ read str

-- | Specification of all recognized command-line options.
flagSpecs :: [OptDescr Flag]
flagSpecs =
  [ Option ['v'] ["verbose"]    (NoArg Verbose)       "Enable verbose output."
  , Option ['h'] ["help"]       (NoArg Help)          "Show this help."
  , Option ['i'] ["iterations"] (ReqArg (fromRight (error "incorrect value") . parseIterations) "N") "How many iterations to run."
  , Option ['p'] ["population"] (ReqArg (fromRight (error "incorrect value") . parsePopulation)   "P") "How large the population is."
  , Option ['t'] ["target"]     (ReqArg Target "T")   "How large the population is."
  ]

-- | Usage banner generated from the flag specs.
usage :: String
usage = usageInfo "Usage: keylay-gen [OPTIONS]" flagSpecs

-- | Validate that no positional (non-option) arguments were passed.
handleNonOptions :: [String] -> [String]
handleNonOptions [] = []
-- handleNonOptions [_] = []
handleNonOptions args = [[i|unexpected args: #{unwords args}|]]

-- | Apply a single parsed flag to a 'Config', overriding the relevant field.
applyFlag :: Flag -> Config -> Config
applyFlag (Iterations n) cfg = cfg { iterations = n }
applyFlag (Population p) cfg = cfg { population = p }
applyFlag (Target str)   cfg = cfg { target = str }
applyFlag Verbose        cfg = cfg { verbose = True }
applyFlag Help           cfg = cfg

-- | Check whether the help flag was requested.
wantsHelp :: [Flag] -> Bool
wantsHelp = isJust . Data.List.find (== Help)

-- | Strip the program name from the argument list (first element).
skimProgPath :: [String] -> [String]
skimProgPath []     = []
skimProgPath (_:xs) = xs

-- | Parse raw CLI arguments into a 'Config'.
--
-- Returns a descriptive error string (including usage info) on failure,
-- or the fully-resolved configuration on success.
parseArgs :: [String] -> Either String Config
parseArgs strs =
  let (options, nonOptions, unrecognized, errors) = getOpt' RequireOrder flagSpecs $ {- skimProgPath -} strs
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
