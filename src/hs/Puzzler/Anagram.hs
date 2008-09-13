
-- | Functions for discovering anagrams.
module Puzzler.Anagram
    ( createDictionary
    , makeDictionary
    , knuth
    , anagrams
    , anagramsPat
    , mbAna )
    where

import Control.Monad( liftM )
import Data.Array.IArray
import Data.ByteString.Char8( ByteString, lines, pack, unpack, readFile )
import Data.Foldable( toList )
import Data.List( foldl' )
import Data.Maybe( isJust )
import Data.IntSet( IntSet )
import Prelude hiding( readFile, lines )
import Text.Regex

import Puzzler.StringTrie( Trie )
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntSet as Set
import qualified Prelude
import qualified Puzzler.StringTrie as Trie

type Words = Array Int ByteString
data Dictionary = Dictionary
    { dictWords :: Words
    , sortWords :: Trie IntSet
    -- ^ Map of sorted strings in `dictWords' to all the corresponding indices
    -- in `dictWords'.
    }

-- | Creates an anagram dictionary from a file of words, one per line.  The
-- words may be compound, as long as there is one per line.
createDictionary :: FilePath -> IO Dictionary
createDictionary path = (makeDictionary . lines) `liftM` readFile path

-- | Makes an anagram dictionary from a list of words.
makeDictionary :: [ByteString] -> Dictionary
makeDictionary ws = Dictionary
    { dictWords = dw
    , sortWords = go end Trie.empty }
  where
    dw = listArray (0, length ws - 1) ws ; (begin, end) = bounds dw
    go i t | i < begin = t
           | otherwise = go (i-1)
                         $! Trie.insertWith Set.union (BS.sort (dw!i)) (Set.singleton i) t

-- | Returns a list of all the anagrams of the given string.
--
-- The algorithm is attributed in various places on the 'net to Knuth in TAOCP
-- Vol. III; hence the name.
knuth :: Dictionary -> ByteString -> [ByteString]
-- Returns all strings whose indices are mapped to by the sorted string input.
knuth a s = map (dw!) . maybeSetToList
            $ {-# SCC "knuth-lookup" #-} Trie.lookup (BS.sort s) sw
  where 
    sw = sortWords a ; dw = dictWords a
    maybeSetToList = maybe [] Set.toList

-- | @anagrams a alphas@ returns anagrams using all the strings in @alphas@.
anagrams :: Dictionary -> [ByteString] -> [ByteString]
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
anagramsPat :: Dictionary -> ByteString -> ByteString -> [ByteString]
anagramsPat a alpha pat = filter matchesPat
                          $ anagrams a (map pack (combinations (BS.length pat) (unpack alpha)))
    where
      matchesPat bs = all pairsSatisfyPat (zip (BS.unpack bs) patBS)
      patBS = BS.unpack pat
  
      pairsSatisfyPat (x,'?') = True
      pairsSatisfyPat (x,y)   = x == y


-- | Escape special regex sequences for the given character.
regexQuoteChar :: Char -> ByteString
regexQuoteChar c = if c == '[' || c == '*' || c == '.' || c == '\\'
                      || c == '?' || c == '+'
                      || c == '^' || c == '$'
                   then BS.pack $ '\\':[c] else BS.pack [c]



-- | An anagramer using @data/mball.txt@.
mbAna :: IO Dictionary
mbAna = createDictionary "data/mball.txt"


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
