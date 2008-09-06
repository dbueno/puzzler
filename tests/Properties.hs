module Properties where

import Data.Char ( chr, ord )
import Data.Maybe( isJust )
import Puzzler.StringTrie( Trie )
import Test.QuickCheck
import qualified Puzzler.StringTrie as Trie


instance Arbitrary Char where
    arbitrary     = choose (32,255) >>= \n -> return (chr n)
    coarbitrary n = variant (ord n)


------------------------------------------------------------------------------
-- Trie

prop_trie_lookup ss =
    all (\s -> isJust $ Trie.lookup s trie) ss'
  where
    ss'  = filter (not . (== "")) ss
    trie = Trie.fromList (zip ss (repeat ()))



