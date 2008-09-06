
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
import Data.List( sort )
import Data.Maybe( isJust )
import Text.Regex

type Dictionary = Array Int String
data Anagramer = Anagramer
    { dictWords :: Dictionary
    , sortWords :: Dictionary }

makeAnagramer :: FilePath -> IO Anagramer
makeAnagramer path = do
    dw <- makeDictionary path
    return $ Anagramer { dictWords = dw, sortWords = amap sort dw }

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
knuth a s = map (dw!)
            -- Find all indices which sort to (sort s).
            $ filter (\i -> sw!i == s') (range . bounds $ sw)
  where 
    s' = sort s
    sw = sortWords a ; dw = dictWords a

-- | @anagrams a alpha subAlphaP anaP@ returns anagrams passing @anaP@ of all
-- substrings of @alpha@ passing @subAlphaP@.
anagrams :: Anagramer -> String -> (String -> Bool) -> (String -> Bool) -> [String]
anagrams a alpha subAlphaP anaP = filter anaP $
    foldr (\ substring as -> knuth a substring ++ as)
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
