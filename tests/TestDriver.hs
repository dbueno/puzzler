module Main where

import Control.Exception( assert )
import Control.Monad
import Properties
import Test.QuickCheck

config = defaultConfig{ configMaxTest = 1000 }
qc = check config

main = do
    shareWords <- words `liftM` readFile "/usr/share/dict/words"
    qc prop_trie_lookup

    putStr $ "prop_trie_lookup with /usr/share/dict/words ("
             ++ show (length shareWords) ++ " words)..."
    assert (prop_trie_lookup shareWords) $ putStrLn "done."
