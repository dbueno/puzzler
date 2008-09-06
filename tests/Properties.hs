module Properties where

import Data.Char ( chr, ord )
import Data.Maybe( isJust )
import Data.List
import Puzzler.StringTrie( Trie )
import Test.QuickCheck
import qualified Data.List as List
import qualified Puzzler.StringTrie as Trie


instance Arbitrary Char where
    arbitrary     = choose (32,127) >>= \n -> return (chr n)
    coarbitrary n = variant (ord n)


------------------------------------------------------------------------------
-- Trie

-- Can lookup all inserted items and they have the right mapping.
prop_trie_lookup ss =
    all (\s -> List.lookup s pairs == Trie.lookup s trie) ss'
  where
    trie  = Trie.fromList pairs
    pairs = zip ss' [1..]
    ss'   = nub $ filter (not . (== "")) ss



