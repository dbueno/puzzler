
-- | Finite maps from strings to values, implemented with the traditional Trie
-- structure.
module Puzzler.StringTrie
    ( Trie
    , lookup
    , strings
    , insert
    , fromList
    , empty )
    where

import Data.Map( Map )
import Prelude hiding( lookup )
import qualified Data.Map as Map

newtype Trie a = Trie { unTrie :: Map Char (Either (Trie a) (Trie a, a)) }
    deriving (Eq, Ord, Show)

lookup :: Trie a -> String -> Bool
lookup = undefined


-- | All the strings starting with the given prefix.
strings :: String -> Trie a -> [String]
strings s (Trie m) = go m s ""
    where
      go m [] pfx =
          -- recursively collect all the entries with "Right" as value
          Map.foldWithKey (\k v ss -> case v of
              Right ((Trie m),_) -> (pfx++[k]) : (go m [] (pfx++[k]) ++ ss)
              Left  (Trie m) -> go m [] (pfx++[k]) ++ ss)
            []
            m

      go m (x:xs) pfx = case Map.lookup x m of
        -- no such word with prefix:
        Nothing -> []
        -- `pfx++[m]' is a word, with suffixes in `m':
        Just (Right ((Trie m),_)) -> (if isLast then (newpfx :) else id) $ go m xs newpfx
        -- `pfx++[m]' is not a word but may have suffixes which are:
        Just (Left (Trie m)) -> go m xs newpfx
        where newpfx = pfx++[x] -- use diff list
              isLast = case xs of [] -> True ; _ -> False


insert :: String -> a -> Trie a -> Trie a
insert [] _ t = t
insert (c:cs) x (Trie m) = Trie (Map.alter myAlter c m)
    where
      -- If `cs' is empty then we form a word.
      cons (t, x) = case cs of [] -> Right (t, x); _ -> Left t

      myAlter Nothing              = Just $ cons (insert cs x empty, x)
      myAlter (Just (Right (t,_))) = Just $ cons (insert cs x t, x)
      myAlter (Just (Left  t))     = Just $ cons (insert cs x t, x)


fromList :: [(String, a)] -> Trie a
fromList = foldr (uncurry insert) empty


empty :: Trie a
empty = Trie Map.empty


trie = fromList [("a", ()), ("an", ())]


