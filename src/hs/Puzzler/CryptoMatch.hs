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
-- the entire list of words, then finding the first letter of the pattern.
-- Next, we find all the (contiguous) suffixes of the list of words that begin
-- with that letter.  It is only among these any possible pattern-matches lie.
-- We test each potential match by looking at each suffix in the original words
-- array and seeing if the appropriate-length string matches the pattern.
findMatches :: SuffixArray -> Pattern -> [ByteString]
findMatches a@(SA{ saSuffixes = suff, saWord = wd }) pat =
    maybe [] collectMatches maybeStartIdx
  where
    collectMatches i =
        let indices = takeWhile suffixStartsFirstLetter [i ..]
            getSubstring j =
                let (start, len) = ((suff!i) - B.length pfx, B.length pat)
                in B.take len . B.drop start $ wd
        in filter (isMatch pat) $ map getSubstring indices

    maybeStartIdx = firstSuffixBeginningWith a firstLetter
    suffixStartsFirstLetter i = wd `B.index` (suff!i) == firstLetter
    (pfx, sfx) = B.span isLetter pat
    Just (firstLetter, _) = B.uncons sfx

isMatch :: Pattern -> ByteString -> Bool
isMatch p s | B.null p && B.null s = True
            | B.null p || B.null s = False
            | otherwise =
                 case (B.head p, B.head s) of
                   ('_', _) -> isMatch (B.tail p) (B.tail s)
                   (c, c')  -> c == c' && isMatch (B.tail p) (B.tail s)

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

