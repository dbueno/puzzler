
-- | Functions for discovering anagrams.
module Puzzler.Anagram
    ( makeAnagramer
    , knuth
    , anagrams
    , anagramsPat
    , shareAna )
    where

import Control.Monad( filterM, liftM )
import Data.Array.IArray
import Data.Foldable( toList )
import Data.List( foldl', sort )
import Data.Maybe( isJust )
import Data.Set( Set )
import Text.Regex

import Puzzler.StringTrie( Trie )
import qualified Data.Set as Set
import qualified Puzzler.StringTrie as Trie

type Dictionary = Array Int String
data Anagramer = Anagramer
    { dictWords :: Dictionary
    , sortWords :: Trie (Set Int)
    -- | Map of sorted strings in `dictWords' to all the corresponding indices
    -- in `dictWords'.
    }

makeAnagramer :: FilePath -> IO Anagramer
makeAnagramer path = do
    dw <- makeDictionary path
    return $ Anagramer
      { dictWords = dw
      , sortWords = fromListMany [ (sort (dw!i), i)
                                 | i <- (range . bounds $ dw) ] }

-- | Create a trie in which equal keys map to a set of all the (possibly
-- distinct) values corresponding to the key.
fromListMany :: (Ord i) => [(String, i)] -> Trie (Set i)
fromListMany assocs =
    foldl' (\t (s,i) -> Trie.insertWith Set.union s (Set.singleton i) t)
      Trie.empty assocs

-- | A dictionary manages a list of words.
makeDictionary :: FilePath -> IO Dictionary
makeDictionary path = do
    dict <- words `liftM` readFile path
    return $ listArray (0, length dict - 1) dict

-- | An anagramer using the words from  @/usr/share/dict/words@.
shareAna :: IO Anagramer
shareAna = makeAnagramer "/usr/share/dict/words"

-- | Returns a list of all the anagrams of the given string.
--
-- The algorithm is attributed in various places on the 'net to Knuth in TAOCP
-- Vol. III; hence the name.
knuth :: Anagramer -> String -> [String]
knuth a s = map (dw!) . toList . maybeToSet $ Trie.lookup sSort sw
  where 
    sSort = sort s
    sw = sortWords a ; dw = dictWords a

    maybeToSet Nothing    = Set.empty
    maybeToSet (Just set) = set

-- | @anagrams a alpha subAlphaP anaP@ returns anagrams passing @anaP@ of all
-- substrings of @alpha@ passing @subAlphaP@.
anagrams :: Anagramer -> String -> (String -> Bool) -> (String -> Bool) -> [String]
anagrams a alpha subAlphaP anaP = filter anaP $
    foldl' (\ as substring -> knuth a substring ++ as)
      []
      (filter subAlphaP $ powerset alpha)

-- | @anagramsPat alpha pat@ returns all anagrams matching the given pattern.
--
-- Patterns may include anagram alphabet letters and ?.  ? Signifies that that
-- location in any string matching the pattern may be any letter.  This function
-- does not confirm that the pattern is valid, and thus one could do all sorts
-- of regex shenanigans with this functions.
anagramsPat :: Anagramer -> String -> String -> [String]
anagramsPat a alpha pat = anagrams a alpha (const True) (isJust . (matchRegex regexp))
  where regexp = mkRegex $ "^" ++ map dotQuestionMarks pat ++ "$"
        dotQuestionMarks = (\c -> case c of '?' -> '.' ; x -> x)
    


------------------------------------------------------------------------------
-- Utilities

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])
