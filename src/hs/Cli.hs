
-- alternative main, for debugging.  No attempt is made to keep this in sync
-- with Main.hs.
module Main where

import Control.Monad( forM_, liftM )
import Data.List( intercalate )
import Foreign
import Foreign.C
import System.Environment( getArgs )

import Glpk
import Glpk.Examples.GlpkExample( problem )
import Glpk.Raw
import Puzzler.Anagram( anagramsPat, shareAna )

shareAnagramer = unsafePerformIO shareAna

main :: IO ()
main = do
    [gramFile] <- getArgs
    putStrLn $ "Reading " ++ gramFile
    gramLines <- lines `liftM` readFile gramFile 
    forM_ gramLines $ \line -> do
      let [letters, pattern] = words line
          grams = anagramsPat shareAnagramer letters pattern
      putStrLn $ intercalate " " (take 10 grams)

