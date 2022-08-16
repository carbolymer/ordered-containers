{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
module Data.MultiMap.Ordered.Strict where

import qualified Data.Map.Ordered.Strict as OM
import Data.Foldable (foldl')
import Control.Monad (join)


-- | A strict multimap which preserves insertion order for keys and values.
-- If you want to use this map, make sure you're ok with asymptotics, see:
-- https://hackage.haskell.org/package/ordered-containers-0.2.2/docs/Data-Map-Ordered.html
type OMultiMap k v = OM.OMap k [v]

instance (Ord k, Semigroup v) => Semigroup (OM.OMap k v) where
  a <> b = OM.unionWithL (const (<>)) a b

instance (Ord k, Semigroup v) => Monoid (OM.OMap k v) where
  mempty = OM.empty

insert :: Ord k => k -> v -> OMultiMap k v -> OMultiMap k v
insert !k !v !mm = insertAll k [v] mm

insertAll :: Ord k => k -> [v] -> OMultiMap k v -> OMultiMap k v
insertAll !k vs !mm = newMap `seq` forceElements `seq` (mm <> newMap)
  where
    forceElements = foldl' (flip seq) () vs
    newMap = OM.singleton (k, vs)

fromList :: Ord k => [(k, v)] -> OMultiMap k v
fromList = foldl' (flip $ uncurry insert) OM.empty

toList :: OMultiMap k v -> [(k, v)]
toList = concatMap (\(k, vs) -> map (k,) vs) . assocs

elements :: OMultiMap k v -> [v]
elements = join . map snd . OM.assocs

insertIfMember :: Ord k => k -> [v] -> OMultiMap k v -> OMultiMap k v
insertIfMember k vs mm
  | OM.member k mm = insertAll k vs mm
  | otherwise = mm

assocs :: OMultiMap k v -> [(k, [v])]
assocs = OM.assocs

filterValue :: Ord k => (v -> Bool) -> OMultiMap k v -> OMultiMap k v
filterValue f = fromList . filter (f . snd) . toList

lookup :: Ord k => k -> OMultiMap k v -> [v]
lookup k = concat . OM.lookup k
