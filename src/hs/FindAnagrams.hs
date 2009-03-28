
-- | Front-end for finding anagrams.
module Main where

import Control.Monad
import Data.List( nub, sort )
import Puzzler.Anagram
import Puzzler.Conf
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO
import System.Log.Logger
import System.Log.Handler.Simple
import Text.Printf

import qualified Data.ByteString.Char8 as B


options :: [OptDescr (Options -> Options)]
options =
  [ Option ['p'] ["pattern"]
    (ReqArg (\s o -> o{ pattern = Just s }) "PATTERN")
    "Specify a pattern for resulting anagrams to match."
  ]

data Options = Options
    { pattern :: Maybe String }
defaultOptions :: Options
defaultOptions = Options{ pattern = Nothing }

main :: IO ()
main = do
    prepareLoggers
    (opts, letters) <- getArgs >>= validateArgv

    dict <- readDictionary "data/mball.txt"
    let result =
          case pattern opts of
            Nothing  -> knuth dict (B.pack letters)
            Just pat -> anagramsPat dict (B.pack letters) (B.pack pat)
    when (null result) $ do
        errorM puzzLog $ printf "No anagrams for '%s'" letters
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
        createDictionary path
     else do
        errorM puzzLog $ printf "No dictionary at '%s'." path
        exitPuzzlerSad

prepareLoggers :: IO ()
prepareLoggers = do
    s <- streamHandler stderr ERROR
    updateGlobalLogger puzzLog (setLevel DEBUG . setHandlers [s])
    infoM puzzLog "*** puzzler started"


