module Puzzler.Anagram where

import Data.List( find, sort )
import Data.Array.IArray
import System.IO.Unsafe( unsafePerformIO )

-- | The contents of @/usr/share/dict/words@, in an array.
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