module Main where

import Control.Exception( assert )
import Control.Monad
import Properties
import Test.QuickCheck
import qualified Data.ByteString.Char8 as BS

config = defaultConfig{ configMaxTest = 1000 }

qc :: Testable a => a -> IO ()
qc = check config

main = do
    -- Trie
--     putStr "prop_trie_lookup: " >> qc prop_trie_lookup
--    mbAllWords <- lines `liftM` readFile "data/mball.txt"
    mbAllWords <- BS.lines `liftM` BS.readFile "/usr/share/dict/words"
    putStrLn $ "prop_trie_lookup with data/mball.txt ("
               ++ show (length mbAllWords) ++ " words)..."
               ++ "done: "
               ++ if (prop_trie_lookup mbAllWords)
                  then "passed." else "FAILED."

    -- Anagrams
--     putStr "prop_anagram_self: "    >> qc prop_anagram_self
--     putStr "prop_anagram_in_dict: " >> qc prop_anagram_in_dict
--     putStr "prop_anagramsPat: "     >> qc prop_anagramsPat



-- With strings:
--     [/Volumes/work/puzzler (git on master)]
--     [149] $ gr ./dist/build/puzzler-test/puzzler-test
--     LENGTH ss' = 234936
--     prop_trie_lookup with data/mball.txt (234936 words)...done: passed.
--     /tmp/growl-cmd.XXXXXXXXXX.lfOiF6ub:
--     ./dist/build/puzzler-test/puzzler-test
--     wall: 58:40.06 (3520.06s)
--     u: 3412.05 k: 17.20
