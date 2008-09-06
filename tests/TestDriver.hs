module Main where

import Properties
import Test.QuickCheck

config = defaultConfig{ configMaxTest = 1000 }
qc = check config

main = do
    qc prop_trie_lookup
