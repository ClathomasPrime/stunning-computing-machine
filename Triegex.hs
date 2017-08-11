{-# LANGUAGE DeriveFunctor #-}

module Triegex where

import Prelude hiding (lookup, elem)
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import Data.Maybe

-- data MiTri k a = MiTri
--   { nil :: [a]
--   , cons :: Map k (MiTri k a)
--   } 

data Trie k a = Trie 
  { trieElems :: [a]
  , trieChildren :: Map k (Trie k a)
  } deriving(Show, Eq, Functor)

type TrieSet k = Trie k ()

lookup :: Ord k => [k] -> Trie k a -> [a]
lookup [] tr = trieElems tr
lookup (k:ks) tr = 
  lookup ks =<< maybeToList (Map.lookup k (trieChildren tr))

toList :: Ord k => Trie k a -> [([k], [a])]
toList tr =
  let rest = Map.toList (toList <$> trieChildren tr)
      phi (k, xs) = map (\(ks, as) -> (k:ks, as)) xs
   in ([], trieElems tr) : concatMap phi rest

flatten :: Ord k => Trie k (Trie k a) -> Trie k a
flatten (Trie elems children) = 
  Trie (trieElems =<< elems) 
    $ foldl (Map.unionWith union) (fmap flatten children) (map trieChildren elems)

sing :: k -> TrieSet k
sing k = Trie [] $ Map.singleton k (Trie [()] Map.empty)

singe :: TrieSet k
singe = Trie [()] Map.empty

singl :: Ord k => [k] -> TrieSet k
singl [] = singe
singl (k:ks) = sing k `cat` singl ks

elem :: Ord k => [k] -> TrieSet k -> Bool
elem ks = not . null . lookup ks

cat :: Ord k => TrieSet k -> TrieSet k -> TrieSet k
cat tr tr' = flatten $ fmap (const tr') tr

union :: Ord k => Trie k a -> Trie k a -> Trie k a
union tr tr'
  = Trie (trieElems tr ++ trieElems tr')
    $ Map.unionWith union (trieChildren tr) (trieChildren tr')

star :: Ord k => TrieSet k -> TrieSet k
star tr = tr `union` (tr `cat` star tr)
