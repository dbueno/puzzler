
-- | Functions for discovering anagrams.
module Puzzler.Anagram
    ( makeAnagramer
    , knuth
    , anagrams
    , shareAna )
    where

import Control.Monad( filterM, liftM )
import Data.List( sort )
import Data.Array.IArray

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

-- | Returns anagrams of all substrings of given size.
anagrams :: Anagramer -> String -> Int -> [String]
anagrams a s size =
    foldr (\ substring as -> knuth a substring ++ as)
      []
      (filter (\s -> length s == size) $ powerset s)


------------------------------------------------------------------------------
-- Utilities

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])
