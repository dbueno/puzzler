
-- | Finite maps from non-empty strings to values, implemented with the
-- traditional Trie structure.
--
-- This structure _does not_ represent the empty list.
module Puzzler.StringTrie
    ( Trie
    , empty
    , insert
    , insertWith
    , lookup
    , fromList
    , fromList' )
    where

import Control.Applicative( (<$>) )
import Control.Parallel.Strategies
import Data.Binary( Binary(..) )
import Data.ByteString.Char8( ByteString, uncons )
import Data.Char( ord )
import Data.IntMap( IntMap )
import Data.List( foldl' )
import Prelude hiding( lookup )

import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap as Map

newtype Trie a = Trie { unTrie :: IntMap (Either (Trie a) (Trie a, a)) }
    deriving (Eq, Ord, Show, Read)
instance NFData (Trie a) where  -- whnf

instance Binary a => Binary (Trie a) where
    put = put . unTrie 
    get = Trie <$> get
      


insertWith :: (NFData a) => (a -> a -> a) -> ByteString -> a -> Trie a -> Trie a
insertWith f bs x t@(Trie m) = case uncons bs of
      Nothing      -> t
      Just (c, cs) -> Trie $! ((Map.alter myAlter (ord c) m) `using` rdeepseq)
        where
          myAlter Nothing               = Just $! cons (insertWith f cs x empty, x)
          myAlter (Just (Left  t))      = Just $! cons (insertWith f cs x t, x)
          myAlter (Just (Right (t,x'))) =
              Just $! Right (insertWith f cs x t, if isLast then f x x' else x')

          -- If `cs' is empty then `c' is the last char of the word to insert;
          -- that is, we're done.
          cons (t, x) | seq t $ seq x $ False = undefined
                      | otherwise             = if isLast then Right (t, x) else Left t
          isLast = BS.null cs


insert :: (NFData a) => ByteString -> a -> Trie a -> Trie a
insert = insertWith const


lookup :: ByteString -> Trie a -> Maybe a
lookup s t = go (unTrie t) (uncons s)
    where
      go m unconsView = do -- Maybe monad
          (x, xs) <- unconsView
          next    <- Map.lookup (ord x) m
          case next of
            Right (Trie m, x) -> if BS.null xs then Just x else go m (uncons xs)
            Left (Trie m)     -> go m (uncons xs)


fromList, fromList' :: (NFData a) => [(ByteString, a)] -> Trie a
fromList  = foldl  (flip $ uncurry insert) empty
fromList' = foldl' (flip $ uncurry insert) empty


empty :: Trie a
empty = Trie Map.empty


