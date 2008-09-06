
-- | Finite maps from strings to values, implemented with the traditional Trie
-- structure.
module Puzzler.StringTrie
    ( Trie
    , lookup
    , strings
    , insert
    , insertWith
    , fromList
    , empty )
    where

import Data.Map( Map )
import Prelude hiding( lookup )
import qualified Data.Map as Map

newtype Trie a = Trie { unTrie :: Map Char (Either (Trie a) (Trie a, a)) }
    deriving (Eq, Ord, Show)

lookup :: String -> Trie a -> Maybe a
lookup s t = case strings s t of
    []  -> Nothing
    (result,x):_ | result == s -> Just x
                 | otherwise   -> Nothing


-- | All the mappings starting with the given prefix, with the most matching one
-- first (?).
strings :: String -> Trie a -> [(String, a)]
strings s (Trie m) = go m s ""
    where
      go m [] pfx =
          -- recursively collect all the entries with "Right" as value
          Map.foldWithKey (\k v ss -> case v of
              Right ((Trie m),x) -> (pfx++[k],x) : (go m [] (pfx++[k]) ++ ss)
              Left  (Trie m) -> go m [] (pfx++[k]) ++ ss)
            []
            m

      go m (x:xs) pfx = case Map.lookup x m of
        -- no such word with prefix:
        Nothing -> []
        -- `pfx++[m]' is a word, with suffixes in `m':
        Just (Right ((Trie m),x)) -> (if isLast then ((newpfx,x) :) else id) $ go m xs newpfx
        -- `pfx++[m]' is not a word but may have suffixes which are:
        Just (Left (Trie m)) -> go m xs newpfx

        where newpfx = pfx++[x] -- use diff list
              isLast = case xs of [] -> True ; _ -> False


insertWith :: (a -> a -> a) -> String -> a -> Trie a -> Trie a
insertWith _ [] _ t = t
insertWith f (c:cs) x (Trie m) = Trie (Map.alter myAlter c m)
    where
      -- If `cs' is empty then `c' is the last char of the word to insert; that
      -- is, we're done.
      cons (t, x) = case cs of [] -> Right (t, x); _ -> Left t

      myAlter Nothing               = Just $ cons (insertWith f cs x empty, x)
      myAlter (Just (Right (t,x'))) = Just $ Right (insertWith f cs x t, f x x')
      myAlter (Just (Left  t))      = Just $ cons (insertWith f cs x t, x)


insert :: String -> a -> Trie a -> Trie a
insert = insertWith (\new_value _old_value -> new_value)


fromList :: [(String, a)] -> Trie a
fromList = foldr (uncurry insert) empty


empty :: Trie a
empty = Trie Map.empty


trie = fromList [ ("an", ())
                , ("a", ()) ]


