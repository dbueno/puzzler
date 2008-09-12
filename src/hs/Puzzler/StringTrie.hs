
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

import Data.ByteString.Char8( ByteString, uncons )
import Data.Map( Map )
import Data.List( foldl' )
import Prelude hiding( lookup )
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map

newtype Trie a = Trie { unTrie :: Map Char (Either (Trie a) (Trie a, a)) }
    deriving (Eq, Ord, Show)


insertWith :: (a -> a -> a) -> ByteString -> a -> Trie a -> Trie a
insertWith f bs x t@(Trie m) = case uncons bs of
      Nothing      -> t
      Just (c, cs) -> Trie (Map.alter myAlter c m)
        where
          myAlter Nothing               = Just $ cons (insertWith f cs x empty, x)
          myAlter (Just (Left  t))      = Just $ cons (insertWith f cs x t, x)
          myAlter (Just (Right (t,x'))) =
              Just $ Right (insertWith f cs x t, if isLast then f x x' else x')

          -- If `cs' is empty then `c' is the last char of the word to insert;
          -- that is, we're done.
          cons (t, x) = if isLast then Right (t, x) else Left t
          isLast = BS.null cs


insert :: ByteString -> a -> Trie a -> Trie a
insert = insertWith const


lookup :: ByteString -> Trie a -> Maybe a
lookup s t = go (unTrie t) (uncons s)
    where
      go m unconsView = do -- Maybe monad
          (x, xs) <- unconsView
          next    <- Map.lookup x m
          case next of
            Right (Trie m, x) -> if BS.null xs then Just x else go m (uncons xs)
            Left (Trie m)     -> go m (uncons xs)


fromList, fromList' :: [(ByteString, a)] -> Trie a
fromList  = foldr (uncurry insert) empty
fromList' = foldl' (flip $ uncurry insert) empty


empty :: Trie a
empty = Trie Map.empty


