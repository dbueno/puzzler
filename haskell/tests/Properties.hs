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

newtype PosString = PosString { unPosString :: String } deriving (Show)
instance Arbitrary PosString where
    arbitrary = sized $ \n -> do
                  l <- choose (1, n+1) -- n is a natural, so n+1 >= 1
                  liftM PosString $ vector l

-- sanity checks
prop_arb_posString ps = length (unPosString ps) > 0
prop_arb_posByteString pbs = BS.length (unPosByteString pbs) > 0
            


------------------------------------------------------------------------------
-- Trie

prop_trie_lookup :: [PosByteString] -> Bool
-- Can lookup all inserted items and they have the right mapping.
prop_trie_lookup ss = prop_trie_lookup_queries ss ss

prop_trie_lookup_queries :: (Foldable f) => [PosByteString] -> f PosByteString -> Bool
prop_trie_lookup_queries queries words =
    all (\s -> maybe False (\i -> s == wordArr!i) $ Trie.lookup (unPosByteString s) trie) queries
  where
    trie    = Trie.fromList' (zipWith (\ (PosByteString s) i -> (s,i)) words' [1..])
    wordArr = listArray (1, length words') words' :: Array Int PosByteString
    words'  = toList words

------------------------------------------------------------------------------
-- Anagrams

newtype AnagramPat = AnagramPat ByteString deriving (Show)
instance Arbitrary AnagramPat where
    arbitrary = sized $ \n ->
        return . AnagramPat . BS.pack $ replicate (n `mod` 4) '?'

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


-- All dictionary words must anagram to themselves.
prop_anagram_self ss =
    all (\s -> s `elem` Anagram.knuth dict s) ss'
  where
    dict = Anagram.makeDictionary ss'
    ss'  = map unPosByteString ss

-- Any found anagram must have come from the original list.
prop_anagram_in_dict ss =
    all (\s -> all (\anag -> anag `elem` ss') $ Anagram.knuth dict s) ss'
  where
    dict = Anagram.makeDictionary ss'
    ss'  = map unPosByteString ss
-- 
prop_anagramsPat ss (AnagramPat p) =
    all (\s -> all (goodMatch s) (Anagram.anagramsPat dict s p)) ss'
--     let Just ce = find (\s -> not $ all (goodMatch s) (Anagram.anagramsPat dict s p)) ss'
--     in (ce, Anagram.anagramsPat dict ce p)
  where
    goodMatch s matchString =
        BS.length matchString == BS.length p
        && all (`BS.elem` s) (BS.unpack matchString)
        && all pairsSatisfyPat (zip (BS.unpack matchString) (BS.unpack p))

    pairsSatisfyPat (x,'?') = True
    pairsSatisfyPat (x,y)   = x == y

    dict = Anagram.makeDictionary ss'
    ss' = map unPosByteString ss
