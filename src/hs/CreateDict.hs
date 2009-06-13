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
  [ Option ['o'] ["output"]
    (ReqArg (\s o -> o{ optOutput = Just s }) "FILE")
    "Send the dictionary to FILE, instead of standard output."
  , Option [] ["overwrite"]
    (NoArg (\o -> o{ optOverwrite = True }))
    "When passed, silently overwrite the output file, if it exists. By default we fail and exit if the output file exists."
  ]

data Options = Options
    { optOutput :: Maybe FilePath
    , optOverwrite :: Bool
    -- ^ Whether to overwrite the output file, if it exists.  Default false.
    }
defaultOptions :: Options
defaultOptions = Options{ optOutput = Nothing
                        , optOverwrite = False }


main :: IO ()
main = do
    prepareLoggers
    (opts, input) <- getArgs >>= validateArgv
    readWords input >>= writeDictionary opts (optOutput opts)
    exitPuzzlerHappy


validateArgv :: [String] -> IO (Options, String)
validateArgv argv = do
    case getOpt Permute options argv of
      (o,[input],[]) -> return (foldl (flip id) defaultOptions o, input)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: CreatDict [OPTION...] input-words-file"


readWords :: FilePath -> IO Dictionary
readWords path = do
     exists <- doesFileExist path
     if exists then do
         debugM puzzLog $ printf "Reading words from '%s' ..." path
         createDictionary path
      else do
         errorM puzzLog $ printf "No file at '%s'." path
         exitPuzzlerSad

-- | Write the given dictionary to the given path.  If the path is @Nothing@,
-- writes it to standard out.  This function also considers the `optOverwrite'
-- command-line option.
writeDictionary :: Options -> Maybe FilePath -> Dictionary -> IO ()
writeDictionary opts maybePath dict = do
    case maybePath of
      Nothing   -> L.putStr (encode $ DictFile dict)
      Just path -> do
         exists <- doesFileExist path
         if exists && not (optOverwrite opts) then do
            errorM puzzLog $ printf "Refusing to overwrite existing file '%s'." path
            exitPuzzlerSad
          else do
            infoM puzzLog $ printf "Saving dictionary to '%s' ..." path
            L.writeFile path (encode $ DictFile dict)
            exitPuzzlerHappy


