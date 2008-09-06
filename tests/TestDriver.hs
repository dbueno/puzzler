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
    shareWords <- words `liftM` readFile "/usr/share/dict/words"
    putStr $ "prop_trie_lookup with /usr/share/dict/words ("
             ++ show (length shareWords) ++ " words)..."
    assert (prop_trie_lookup shareWords) $ putStrLn "done."

    -- Anagrams
    putStr "prop_anagram_self: " >> qc prop_anagram_self

