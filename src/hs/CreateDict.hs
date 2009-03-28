module Main where

import Control.Monad
import Data.Binary( encode )
import Puzzler.Anagram
import Puzzler.Binary
import Puzzler.Conf
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO
import System.Log.Logger
import Text.Printf

import qualified Data.ByteString.Lazy  as L
import qualified Data.Binary     as Bin

options :: [OptDescr (Options -> Options)]
options =
  [ 
  ]

data Options = Options { }
defaultOptions :: Options
defaultOptions = Options{ }


main :: IO ()
main = do
    prepareLoggers
    (opts, input, output) <- getArgs >>= validateArgv
    d <- readWords input
    d `seq` writeDictionary output d
    exitPuzzlerHappy


validateArgv :: [String] -> IO (Options, String, String)
validateArgv argv = do
    case getOpt Permute options argv of
      (o,[input, output],[]) -> return (foldl (flip id) defaultOptions o, input, output)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: CreatDict [OPTION...] input-words-file output-dict-file"


readWords :: FilePath -> IO Dictionary
readWords path = do
     exists <- doesFileExist path
     if exists then do
         debugM puzzLog $ printf "Reading words from '%s' ..." path
         createDictionary path
      else do
         errorM puzzLog $ printf "No dictionary at '%s'." path
         exitPuzzlerSad

writeDictionary :: FilePath -> Dictionary -> IO ()
writeDictionary path dict = do
--      exists <- doesFileExist path
--      if exists then do
--          errorM puzzLog $ printf "Cowardly refusing to overwrite existing file '%s'." path
--          exitPuzzlerSad
--       else do
     do
         infoM puzzLog $ printf "Saving dictionary to '%s' ..." path
         L.writeFile path (encode $ DictFile dict)
         exitPuzzlerHappy


