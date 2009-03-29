module Puzzler.CryptoMatch where

import Control.Applicative( (<$>), (<*>) )
import Data.Array.IArray
import Data.Binary( Binary(..) )
import Data.ByteString.Char8( ByteString )
import Data.Char( isLetter )
import Data.List( foldl' )
import Data.Maybe( isJust )
import Data.IntSet( IntSet )
import Prelude hiding( readFile, lines )
import Puzzler.SuffixArray
import Text.Regex

import qualified Data.Binary as Bin
import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as Set
import qualified Prelude

-- | A string with at least one letter.
type Pattern = ByteString


-- | This is done by constructing the suffix array for the string concatenating
-- the entire list of words, then finding the longest suffix of the pattern that
-- starts with a letter (not an underscore).  Next, we find all the (contiguous)
-- suffixes that begin with that letter.  It is only among these any possible
-- pattern-matches lie.  We test each potential match by looking at each suffix
-- in the original words array and seeing if the appropriate-length string
-- matches the pattern.
{-findMatches :: SuffixArray -> Pattern -> [ByteString]
findMatches a@(SA{ suffixes = suff, words = wds }) pat =
    maybe [] collectMatches maybeStartIdx
  where
    collectMatches i =
        let potentialMatch = wds

    maybeStartIdx = firstSuffixBeginningWith a firstLetter
    (pfx, firstLetter:sfx) = B.span isLetter pat-}

-- | Returns the index into the suffix array that starts with the given
-- character.  Uses binary search.
firstSuffixBeginningWith :: SuffixArray -> Char -> Maybe Int
firstSuffixBeginningWith sa c = findFirst <$> go one n
  where
    go lo hi | hi < lo = Nothing
    go lo hi =
        let mid = lo + ((hi - lo) `div` 2)
            c_i = firstCharOfSuffix mid
        in if c_i > c then
               go lo (mid-1)
           else if c_i < c then
               go (mid+1) hi
           else
               Just mid

    findFirst 0 = 0
    findFirst i = if firstCharOfSuffix (i-1) == c then findFirst (i-1) else i
               

    firstCharOfSuffix i = wd `B.index` (suff!i)
    suff = saSuffixes sa
    wd = saWord sa
    (one, n) = bounds suff

