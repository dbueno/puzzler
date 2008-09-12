module Main where

import Control.Exception( assert )
import Control.Monad
import Properties
import Test.QuickCheck

config = defaultConfig{ configMaxTest = 1000 }

qc :: Testable a => a -> IO ()
qc = check config

main = do
    -- Trie
    putStr "prop_trie_lookup: " >> qc prop_trie_lookup
    mbAllWords <- words `liftM` readFile "data/mball.txt"
    putStr $ "prop_trie_lookup with data/mball.txt ("
             ++ show (length mbAllWords) ++ " words)..."
    assert (prop_trie_lookup mbAllWords)) $ putStrLn "done."

    -- Anagrams
    putStr "prop_anagram_self: "    >> qc prop_anagram_self
    putStr "prop_anagram_in_dict: " >> qc prop_anagram_in_dict
--     putStr "prop_anagramsPat: "     >> qc prop_anagramsPat

