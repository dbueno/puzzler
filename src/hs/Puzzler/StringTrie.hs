module Puzzler.StringTrie where

import Data.Map( Map )
import qualified Data.Map as Map

newtype Trie = Trie { unTrie :: Map Char (Either Trie Trie) }
    deriving (Eq, Ord, Show)

lookup :: Trie -> String -> Bool
lookup = undefined


-- | All the strings starting with the given prefix.
strings :: String -> Trie -> [String]
strings s (Trie m) = go m s ""
    where
      go m [] pfx =
          -- recursively collect all the entries with "Right" as value
          Map.foldWithKey (\k v ss -> case v of
              Right (Trie m) -> (pfx++[k]) : (go m [] (pfx++[k]) ++ ss)
              Left  (Trie m) -> go m [] (pfx++[k]) ++ ss)
            []
            m

      go m (x:xs) pfx = case Map.lookup x m of
        -- no such word with prefix:
        Nothing -> []
        -- `pfx++[m]' is a word, with suffixes in `m':
        Just (Right (Trie m)) -> (if isLast then (newpfx :) else id) $ go m xs newpfx
        -- `pfx++[m]' is not a word but may have suffixes which are:
        Just (Left (Trie m)) -> go m xs newpfx
        where newpfx = pfx++[x] -- use diff list
              isLast = case xs of [] -> True ; _ -> False


insert :: String -> Trie -> Trie
insert [] t = t
insert (c:cs) (Trie m) = Trie (Map.alter myAlter c m)
    where
      cons = case cs of [] -> Right ; _ -> Left
      myAlter Nothing = Just . cons $ insert cs empty
      myAlter (Just (Right t)) = Just $ cons (insert cs t)
      myAlter (Just (Left  t)) = Just $ cons (insert cs t)


fromList :: [String] -> Trie
fromList = foldr insert empty


empty :: Trie
empty = Trie Map.empty


trie = fromList ["a", "an"]


