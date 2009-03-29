module Puzzler.SuffixArray where

import Control.Applicative( (<$>), (<*>) )
--import Data.Array
import Data.Array.Unboxed
--import Data.Array.IArray
import Data.Binary( Binary(..) )
import Data.ByteString.Char8( ByteString )
import Data.IntSet( IntSet )
import Data.List
import Data.Maybe( isJust )
import Data.Ord( comparing )
import Prelude hiding( readFile, lines )
import Text.Regex

import qualified Data.Binary as Bin
import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as Set
import qualified Prelude


type Word = ByteString

data SuffixArray = SA
    { saWord     :: !Word
    , saSuffixes :: !(UArray Int Int)
      -- ^ In a suffix array sa, words[sa!n .. k] is the nth suffix of saWords
      -- (beginning at 1) in lexicographic order, where the words ByteString has
      -- length k+1.
    }
    deriving (Eq, Ord, Show)

-- instance Binary SuffixArray where
--     put sa = put (words sa) >> put (suffixs sa)
--     get = SA . get

-- | Build a suffix array from a word.  Note that the word can come from
-- multiple words separated by NULs.
buildSuffixArray :: Word -> SuffixArray
buildSuffixArray wd = SA
    { saWord     = wd
    , saSuffixes = array (1, B.length wd)
                 . filter ((/= 0) . fst)
                 . zip [0 ..]
                 . map fst
                 . sortBy (comparing snd)
                 . zip [0 ..]
                 . B.tails
                 $ wd }
