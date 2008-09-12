
-- | Functions for discovering anagrams.
module Puzzler.Anagram
    ( createDictionary
    , makeDictionary
    , knuth
    , anagrams
    , anagramsPat
    , mbSingleAna )
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

type Words = Array Int String
data Dictionary = Dictionary
    { dictWords :: Words
    , sortWords :: Trie (Set Int)
    -- ^ Map of sorted strings in `dictWords' to all the corresponding indices
    -- in `dictWords'.
    }

-- | Creates an anagram dictionary from a file of words, one per line.
createDictionary :: FilePath -> IO Dictionary
createDictionary path = (makeDictionary . lines) `liftM` readFile path

-- | Makes an anagram dictionary from a list of words.
makeDictionary :: [String] -> Dictionary
makeDictionary ws = Dictionary
    { dictWords = dw
    , sortWords = fromListMany [ (sort (dw!i), i) | i <- (range . bounds $ dw) ] }
  where
    dw = listArray (0, length ws - 1) ws

-- | Create a trie in which equal keys map to a set of all the (possibly
-- distinct) values corresponding to the key.
fromListMany :: (Ord i) => [(String, i)] -> Trie (Set i)
fromListMany assocs =
    foldl' (\t (s,i) -> Trie.insertWith Set.union s (Set.singleton i) t)
      Trie.empty assocs

-- | Returns a list of all the anagrams of the given string.
--
-- The algorithm is attributed in various places on the 'net to Knuth in TAOCP
-- Vol. III; hence the name.
knuth :: Dictionary -> String -> [String]
-- Returns all strings whose indices are mapped to by the sorted string input.
knuth a s = map (dw!) . maybeSetToList $ {-# SCC "knuth-lookup" #-} Trie.lookup (sort s) sw
  where 
    sw = sortWords a ; dw = dictWords a
    maybeSetToList = maybe [] toList

-- | @anagrams a alpha subAlphaP anaP@ returns anagrams passing @anaP@ of all
-- each string in @alpha@ passing @subAlphaP@.
anagrams :: Dictionary -> [String] -> [String]
anagrams a alphas =
    foldl' (\ as substring -> knuth a substring ++ as)
      []
      alphas

-- | @anagramsPat alpha pat@ returns all anagrams matching the given pattern.
--
-- Patterns may include anagram alphabet letters and ?.  ? Signifies that that
-- location in any string matching the pattern may be any letter.  This function
-- does not confirm that the pattern is valid, and thus one could do all sorts
-- of regex shenanigans with this functions.
anagramsPat :: Dictionary -> String -> String -> [String]
anagramsPat a alpha pat = filter (isJust . (matchRegex regexp))
                          $ anagrams a (combinations (length pat) alpha)
  where regexp = mkRegex $ "^" ++ concatMap dotQuestionMarks pat ++ "$"
        dotQuestionMarks c = case c of '?' -> "." ; x -> regexQuoteChar x


-- | Escape special regex sequences for the given character.
regexQuoteChar :: Char -> String
regexQuoteChar c = if c == '[' || c == '*' || c == '.' || c == '\\'
                      || c == '?' || c == '+'
                      || c == '^' || c == '$'
                   then '\\':[c] else [c]

regexQuote :: String -> String
regexQuote = concatMap regexQuoteChar




-- | An anagramer using the words from  @/usr/share/dict/words@.
mbSingleAna :: IO Dictionary
mbSingleAna = createDictionary "data/mbsingle.txt"


------------------------------------------------------------------------------
-- Utilities

-- From the thread that began here:
-- http://www.haskell.org/pipermail/haskell-cafe/2003-June/004463.html

-- Hopefully this definition is constant space
powerset       :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = xss /\/ map (x:) xss
    where xss = powerset xs

(/\/)        :: [a] -> [a] -> [a]
[]     /\/ ys = ys
(x:xs) /\/ ys = x : (ys /\/ xs)


-- |Combinations of n elements from a list, each being returned in the
--  order that they appear in the list.
combinations :: Int -> [a] -> [[a]]
combinations n xs = go n (length xs) xs
  where
    go _ _ [] = []
    go n len as@(ah:at)
        | n <= 0    = [[]]
        | n >  len  = []
        | n == len  = [as]
        | otherwise = (map (ah:) $ go (n-1) (len-1) at)
                      ++ go n (len-1) at
