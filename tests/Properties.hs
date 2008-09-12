module Properties where

import Control.Monad( liftM )
import Data.Array.IArray
import Data.ByteString.Char8( ByteString )
import Data.Char ( chr, ord )
import Data.Foldable( Foldable, toList )
import Data.List
import Test.QuickCheck
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import qualified Puzzler.StringTrie as Trie
import qualified Puzzler.Anagram as Anagram

import Debug.Trace

instance Arbitrary Char where
    arbitrary     = choose (32,127) >>= \n -> return (chr n)
    coarbitrary n = variant (ord n)

instance Arbitrary ByteString where
    arbitrary = sized sizedBS
      where
        sizedBS 0 = return $ BS.pack ""
        sizedBS n = do
            c <- arbitrary
            bs <- sizedBS (n-1)
            return $ BS.cons c bs

newtype PosByteString = PosByteString { unPosByteString :: ByteString }
    deriving (Eq, Ord, Show)
instance Arbitrary PosByteString where
    arbitrary = liftM (PosByteString . BS.pack . unPosString) arbitrary
            


------------------------------------------------------------------------------
-- Trie

prop_trie_lookup :: [PosByteString] -> Bool
-- Can lookup all inserted items and they have the right mapping.
prop_trie_lookup ss = prop_trie_lookup1 ss ss

prop_trie_lookup1 :: (Foldable f) => [PosByteString] -> f PosByteString -> Bool
prop_trie_lookup1 queries words =
    all (\s -> maybe False (\i -> s == wordArr!i) $ Trie.lookup (unPosByteString s) trie) queries
  where
    trie    = Trie.fromList' (zipWith (\ (PosByteString s) i -> (s,i)) words' [1..])
    wordArr = listArray (1, length words') words' :: Array Int PosByteString
    words'  = toList words

------------------------------------------------------------------------------
-- Anagrams

prop_anagram_self ss =
    all (\s -> s `elem` Anagram.knuth dict s) ss'
  where
    dict = Anagram.makeDictionary ss'
    ss'  = filter (not . BS.null) ss

prop_anagram_in_dict ss =
    all (\s -> all (\anag -> anag `elem` ss) $ Anagram.knuth dict s)
  where
    dict = Anagram.makeDictionary ss'
    ss'  = filter (not . BS.null) ss

newtype AnagramPat = AnagramPat ByteString deriving (Show)
instance Arbitrary AnagramPat where
    arbitrary = return . AnagramPat . BS.pack $ replicate 4 '?'

newtype PosString = PosString { unPosString :: String } deriving (Show)
instance Arbitrary PosString where
    arbitrary = sized $ \n -> do
                  l <- choose (1, n+1) -- n is a natural, so n+1 >= 1
                  liftM PosString $ vector l

prop_arb_posString ps = length (unPosString ps) > 0
prop_arb_posByteString pbs = BS.length (unPosByteString pbs) > 0

-- | A `PosString' paired with a compatible pattern.
data LowerAlphaStrPat = LowerAlphaStrPat PosString String -- string pat
                        deriving (Show)
instance Arbitrary LowerAlphaStrPat where
    arbitrary = sized $ \n -> do
                          s <- arbitrary
                          i <- arbitrary
                          return $ LowerAlphaStrPat s
                                   (replicate (i `mod` length (unPosString s)) '?')

prop_lowerAlphaStrPat (LowerAlphaStrPat ps pat) =
    length (unPosString ps) >= length pat
-- 
prop_anagramsPat ss (AnagramPat p) =
    all (\s -> all (goodMatch s) (Anagram.anagramsPat dict s p)) ss'
  where
    goodMatch s matchString =
        BS.sort s == BS.sort matchString
        && all pairsSatisfyPat (zip (BS.unpack matchString) (BS.unpack p))

    pairsSatisfyPat (x,'?') = True
    pairsSatisfyPat (x,y)   = x == y

    dict = Anagram.makeDictionary ss'
    ss' = map unPosByteString ss
