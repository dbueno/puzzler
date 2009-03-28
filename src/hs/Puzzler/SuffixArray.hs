module SuffixArray where

import Control.Applicative( (<$>), (<*>) )
import Data.Array.IArray
import Data.Binary( Binary(..) )
import Data.ByteString.Char8( ByteString )
import Data.List( foldl' )
import Data.Maybe( isJust )
import Data.IntSet( IntSet )
import Prelude hiding( readFile, lines )
import Text.Regex

import qualified Data.Binary as Bin
import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as Set
import qualified Prelude


-- | Array indexing starts at 1.
type Words = Array Int ByteString

newtype SuffixArray = SA{ unSA :: UArray Int Int } deriving (Eq, Ord, Show)

data SuffixMap a = SM{ smArray :: !SuffixArray -- array of suffixes of sorted words
                     , smWords :: !Words
                     , smMap   :: !(Array Int a) -- map of suffix idx in smArray to value
                     }

instance Data.Binary where
    put = put . unSA
    get = SA . get

buildSuffixArray :: Words -> SuffixArray
buildSuffixArray wds =
      SA
    . array (bounds wds)
    . sortBy (comparing snd)
    . zip [one ..]
    . tails
    $ elems wds
  where
    (one, n) = bounds wds
