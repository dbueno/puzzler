
-- | Given a pattern with underscores and letters, print out every word that
-- fits the pattern.  For example, if _n_ was given, ''and'' would fit.
module Main where

import Control.Applicative
import Control.Monad
import Data.Binary( decodeFile )
import Data.List( nub, sort )
import Puzzler.Binary
import Puzzler.Anagram
import Puzzler.Conf
import Puzzler.CryptoMatch
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO
import System.Log.Logger
import Text.Printf

options :: [OptDescr (Options -> Options)]
options =
  [ 
  ]

data Options = Options
    {  }
defaultOptions :: Options
defaultOptions = Options{  }

main :: IO ()
main = do
    prepareLoggers
    (opts, pattern) <- getArgs >>= validateArgv

    




validateArgv :: [String] -> IO (Options, String)
validateArgv argv =
    case getOpt Permute options argv of
      (o,[letters],[]  ) -> return (foldl (flip id) defaultOptions o, letters)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: CryptoWord [OPTION...] pattern"

