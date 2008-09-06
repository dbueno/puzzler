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
    all (\s -> if not (s == "")
               then isJust $ Trie.lookup s trie
               else True) ss
  where
    trie = Trie.fromList (zip ss (repeat ()))



