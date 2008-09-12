
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

import Data.Char( ord )
import Data.IntMap( IntMap )
import Data.List( foldl' )
import Prelude hiding( lookup )
import qualified Data.IntMap as Map

newtype Trie a = Trie { unTrie :: IntMap (Either (Trie a) (Trie a, a)) }
    deriving (Eq, Ord, Show)


insertWith :: (a -> a -> a) -> String -> a -> Trie a -> Trie a
insertWith _ [] _ t = t
insertWith f (c:cs) x (Trie m) = Trie (Map.alter myAlter (ord c) m)
    where
      -- If `cs' is empty then `c' is the last char of the word to insert; that
      -- is, we're done.
      cons (t, x) = if isLast then Right (t, x) else Left t
      isLast = case cs of [] -> True; _ -> False

      myAlter Nothing               = Just $ cons (insertWith f cs x empty, x)
      myAlter (Just (Left  t))      = Just $ cons (insertWith f cs x t, x)
      myAlter (Just (Right (t,x'))) =
          Just $ Right (insertWith f cs x t, if isLast then f x x' else x')


insert :: String -> a -> Trie a -> Trie a
insert = insertWith const


lookup :: String -> Trie a -> Maybe a
lookup s t = go (unTrie t) s
    where
      go m []     = Nothing
      go m (x:xs) = case Map.lookup (ord x) m of
        Nothing                   -> Nothing
        Just (Right ((Trie m),x)) -> if isLast then Just x else go m xs
        Just (Left (Trie m))      -> go m xs

        where isLast = case xs of [] -> True ; _ -> False


fromList, fromList' :: [(String, a)] -> Trie a
fromList  = foldr (uncurry insert) empty
fromList' = foldl' (\ t (s,x) -> insert s x t) empty


empty :: Trie a
empty = Trie Map.empty


