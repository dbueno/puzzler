module Properties where

import Control.Monad( liftM )
import Data.ByteString.Char8( ByteString )
import Data.Char ( chr, ord )
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
            


------------------------------------------------------------------------------
-- Trie

prop_trie_lookup :: [ByteString] -> Bool
-- Can lookup all inserted items and they have the right mapping.
prop_trie_lookup ss =
    all (\s -> List.lookup s pairs == Trie.lookup s trie) ss'
  where
    trie  = Trie.fromList' pairs
    pairs = trace ("LENGTH ss' = " ++ show (length ss')) $ zip ss' [1..]
    ss'   = nub $ filter (not . BS.null) ss

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
    ss' = filter (not . BS.null) ss

newtype AnagramPat = AnagramPat String deriving (Show)
instance Arbitrary AnagramPat where
    arbitrary = sized $ \n -> return . AnagramPat $ replicate 4 '?'

newtype PosString = PosString { unPosString :: String } deriving (Show)
instance Arbitrary PosString where
    arbitrary = sized $ \n -> do
                  l <- choose (1, n+1)
                  liftM PosString $ vector l

prop_arb_posString ps = length (unPosString ps) > 0

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

-- ["~@[!cJe","l3cl0Q","_Yk9ac\DELf9|/foU['U\"'>","hD8","pqi}Qx:qBE.M'8Vo'X(<LqbK'*|","=x_w^%7}`&4x@z[ffE:c","DmYh6#C;A,^1# )ES,",":1f?3.a^qW","^?","kv;H]pqv+R","CJF","vU","!=(Mnw7L","i7.w'Y DHz3t ,7{PT6ot)H9Az","c$v1i^je5+[q4\\","3O","{hqxf46dn","","v2nN22nE~3k80ibPks0u)$oK<7)i","^F/;\\Zk<Nc~Jq'r !0<A\\","sh:\"MW,(Hv1b/al`[r&","4","t=","iKo$","wHeT]~D6y8pilF"]
-- AnagramPat "????"


-- prop_anagramsPat ss (AnagramPat p) =
--     all (\s -> all (goodMatch s) (Anagram.anagramsPat dict s p)) ss'
--   where
--     goodMatch s matchString =
--         sort s == sort matchString
--         && all pairsSatisfyPat (zip matchString p)

--     pairsSatisfyPat (x,'?') = True
--     pairsSatisfyPat (x,y)   = x == y

--     dict = Anagram.makeDictionary ss'
--     ss' = filter (not . BS.null) ss
