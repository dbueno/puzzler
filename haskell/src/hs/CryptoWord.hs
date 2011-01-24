
-- | Given a pattern with underscores and letters, print out every word that
-- fits the pattern.  For example, if _n_ was given, ''and'' would fit.
module Main where

import Control.Applicative( (<$>) )
import Control.Monad
import Data.Binary( decodeFile )
import Data.ByteString.Char8( ByteString )
import Data.List( nub, sort )
import Puzzler.Binary
import Puzzler.Anagram
import Puzzler.Conf
import Puzzler.CryptoMatch
import Puzzler.SuffixArray
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO
import System.Log.Logger
import Text.Printf

import qualified Data.ByteString.Char8 as B

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['f'] ["file"]
    (ReqArg (\s o -> o{ optWords = Just s }) "FILE")
    "Read words from FILE."
  ]

data Options = Options
    { optWords :: Maybe FilePath
      -- ^ Nothing means read from stdin.
    }
defaultOptions :: Options
defaultOptions = Options
    { optWords = Nothing }

main :: IO ()
main = do
    prepareLoggers
    (opts, pattern) <- validateArgv =<< getArgs
    suffArr <- (buildSuffixArray . sepWords . B.lines)
               <$> maybe B.getContents B.readFile (optWords opts)
    mapM_ B.putStrLn $ findMatches suffArr (B.pack pattern)
    exitPuzzlerHappy

-- | Separates the words with nulls.  The separator must sort strictly less than
-- any word element (letter or space is what I'm assuming).
sepWords :: [ByteString] -> ByteString
sepWords = B.concat . map (`B.snoc` '\0')


validateArgv :: [String] -> IO (Options, String)
validateArgv argv =
    case getOpt Permute options argv of
      (o,[letters],[]  ) -> return (foldl (flip id) defaultOptions o, letters)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: CryptoWord [OPTION...] pattern"

