module Puzzler.Anagram
    ( knuth
    , anagrams )
    where

import Control.Monad( filterM )
import Data.List( find, sort )
import Data.Array.IArray
import System.IO.Unsafe( unsafePerformIO )

-- | The contents of @/usr/share/dict/words@, in an array.
--
-- This CAF uses `unsafePerformIO'.
dictWords :: Array Int String
dictWords =
    let wds = words . unsafePerformIO $ readFile "/usr/share/dict/words"
    in listArray (0, length wds - 1) wds

-- | `dictWords', but with each string entry sorted.
sortWords = amap sort dictWords

-- | Returns a list of all the anagrams of the given string.
knuth :: String -> [String]
knuth s = 
    let s' = sort s
        anagramIdxs = filter (\i -> sortWords!i == s') (range . bounds $ sortWords)
    in map (dictWords!) anagramIdxs

-- | Returns anagrams of all substrings of given size.
anagrams :: String -> Int -> [String]
anagrams s size =
    foldr (\ substring as -> knuth substring ++ as)
      []
      (filter (\s -> length s == size) $ powerset s)


powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])
