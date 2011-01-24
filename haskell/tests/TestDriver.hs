module Main where

import Control.Exception( assert )
import Control.Monad
import Data.Array.IArray
import Data.Maybe
import Properties
import System.IO
import Test.QuickCheck
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set
import qualified Puzzler.Anagram as Anagram
import qualified Puzzler.StringTrie as Trie

config = defaultConfig{ configMaxTest = 1000 }

qc :: Testable a => a -> IO ()
qc = check config

qcn :: Testable a => Int -> a -> IO ()
qcn i = check (defaultConfig{ configMaxTest = i })

main = do

    -- Generators
    putStr "prop_arb_posString: "     >> quickCheck prop_arb_posString
    putStr "prop_arb_posByteString: " >> quickCheck prop_arb_posByteString
    putStr "prop_lowerAlphaStrPat: "  >> quickCheck prop_lowerAlphaStrPat

    -- Trie
    putStr "prop_trie_lookup: " >> qc prop_trie_lookup
    let file = "data/mball.txt"
--     let file = "/usr/share/dict/words"
    fileWords <- (map PosByteString . BS.lines) `liftM` BS.readFile file
    liftM (\d -> Anagram.anagramsPat d "aoeu" "?") (Anagram.createDictionary file) -- to load the dictionary
    putStr $ "prop_trie_lookup with words file " ++ file ++ " ("
             ++ show (length fileWords) ++ " words)..."
    hFlush stdout
    
    putStrLn $ "done: "
               ++ if prop_trie_lookup_queries fileWords (Set.fromList fileWords)
                  then "passed." else "FAILED."

    -- Anagrams
    putStr "prop_anagram_self: "    >> qc prop_anagram_self
    putStr "prop_anagram_in_dict: " >> qc prop_anagram_in_dict
    putStr "prop_anagramsPat: "     >> qcn 200 prop_anagramsPat



-- With strings:
--     [/Volumes/work/puzzler (git on master)]
--     [149] $ gr ./dist/build/puzzler-test/puzzler-test
--     LENGTH ss' = 234936
--     prop_trie_lookup with data/mball.txt (234936 words)...done: passed.
--     /tmp/growl-cmd.XXXXXXXXXX.lfOiF6ub:
--     ./dist/build/puzzler-test/puzzler-test
--     wall: 58:40.06 (3520.06s)
--     u: 3412.05 k: 17.20
