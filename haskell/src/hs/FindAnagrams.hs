
-- | Front-end for finding anagrams.
module Main where

import Control.Applicative
import Control.Monad
import Data.Binary( decodeFile )
import Data.List( nub, sort )
import Puzzler.Binary
import Puzzler.Anagram
import Puzzler.Conf
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO
import System.Log.Logger
import Text.Printf

import qualified Data.ByteString.Char8 as B
import qualified Text.Regex as Rx


options :: [OptDescr (Options -> Options)]
options =
  [ Option ['p'] ["pattern"]
    (ReqArg (\s o -> o{ optPattern = Just s }) "PATTERN")
    "Specify a PATTERN for resulting anagrams to match."
  , Option ['d'] ["dictionary"]
    (ReqArg (\s o -> o{ optDictionary = s }) "FILE")
    "Use the dictionary (list of words) from the given FILE."
  ]

data Options = Options
    { optPattern :: Maybe String
    , optDictionary :: FilePath }
defaultOptions :: Options
defaultOptions = Options{ optPattern = Nothing
                        , optDictionary = "data/mball.puz" }

main :: IO ()
main = do
    prepareLoggers
    (opts, letters) <- getArgs >>= validateArgv

    dict <- readDictionary (optDictionary opts)
    let result =
          case optPattern opts of
            Nothing  -> anagramsPat dict (B.pack letters) (Rx.mkRegex ".*")
            Just pat -> anagramsPat dict (B.pack letters) (Rx.mkRegex pat)
    when (null result) $ do
        infoM puzzLog $ printf "No anagrams for '%s'" letters
        exitPuzzlerHappy
    forM_ (nub . sort $ result) B.putStrLn
    exitPuzzlerHappy
      


validateArgv :: [String] -> IO (Options, String)
validateArgv argv =
    case getOpt Permute options argv of
      (o,[letters],[]  ) -> return (foldl (flip id) defaultOptions o, letters)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: FindAnagrams [OPTION...] word"

readDictionary :: FilePath -> IO Dictionary
readDictionary path = do
    exists <- doesFileExist path
    if exists then do
        debugM puzzLog $ printf "Reading dictionary from '%s' ..." path
        unDictFile <$> decodeFile path
     else do
        errorM puzzLog $ printf "No dictionary at '%s'." path
        exitPuzzlerSad


