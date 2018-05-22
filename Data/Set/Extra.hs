{-# LANGUAGE CPP #-}

module Data.Set.Extra
    ( module Set
    , mapM
    , mapM_
    , filterM
    , catMaybes
    , mapMaybe
    , flatten
    , concatMap
    , concatMapM
    , any
    , all
    , or
    , and
    , ss
    , toSS
    , fromSS
    , ssMapM
    , distrib
    , cartesianProduct
    , groupBy
    , powerset
    , partitionM
    , unzip
    , gFind
    ) where

import qualified Control.Monad as List (mapM, mapM_, filterM, foldM)
import Control.Monad.State ()
import qualified Data.Foldable as Foldable (all, any, and, or)
import Data.Map as Map (Map, insertWith, empty)
import Data.Set as Set
import Data.Set.ExtraG
import qualified Data.List as List
--import qualified Data.Maybe
import Prelude hiding (mapM, mapM_, unzip, all, any, map, filter, null, concatMap, and, or)
--import qualified Prelude

mapM :: (Monad m, Ord b) => (a -> m b) -> Set a -> m (Set b)
mapM f s = List.mapM f (toList s) >>= return . fromList

mapM_ :: (Monad m, Ord b) => (a -> m b) -> Set a -> m ()
mapM_ f s = List.mapM_ f (toList s)

filterM :: (Ord a, Monad m) => (a -> m Bool) -> Set a -> m (Set a)
filterM p s = List.filterM p (toList s) >>= return . fromList

catMaybes :: Ord a => Set (Maybe a) -> Set a
catMaybes sm = fold (\ mx s -> maybe s (`insert` s) mx) Set.empty sm

mapMaybe :: (Ord a, Ord b) => (a -> Maybe b) -> Set a -> Set b
mapMaybe f s = catMaybes (Set.map f s)

flatten :: Ord a => Set (Set a) -> Set a
flatten ss' = fold union Set.empty ss'
--flatten = unions . toList

concatMap :: (Ord a, Ord b) => (a -> Set b) -> Set a -> Set b
concatMap f s = flatten (Set.map f s)

concatMapM :: (Monad m, Ord a, Ord b) => (a -> m (Set b)) -> Set a -> m (Set b)
concatMapM f s = mapM f s >>= return . flatten

any :: Ord a => (a -> Bool) -> Set a -> Bool
any = Foldable.any

{-
anyM :: Monad m => (a -> m (Maybe Bool)) -> Set a -> m (Maybe Bool)
anyM p s =
    List.mapM p (toList s) >>= return . Data.Maybe.catMaybes >>= return . chk
    where chk [] = Nothing
          chk ys = Just (Prelude.or ys)
-}

all :: Ord a => (a -> Bool) -> Set a -> Bool
all = Foldable.all

or :: Set Bool -> Bool
or = Foldable.or

and :: Set Bool -> Bool
and = Foldable.and

-- |Create a singleton set containing a singleton set of a.
ss :: Ord a => a -> Set (Set a)
ss = singleton . singleton

-- |Turn a list of lists into a set of sets.
toSS :: Ord a => [[a]] -> Set (Set a)
toSS = fromList . List.map fromList

fromSS :: Ord a => Set (Set a) -> [[a]]
fromSS = List.map toList . toList

ssMapM :: (Monad m, Ord a, Ord b) => (a -> m b) -> Set (Set a) -> m (Set (Set b))
ssMapM f s = List.mapM (List.mapM f) (fromSS s) >>= return . toSS

-- | distrib {a, b, c} {d, e, f} -> {a+d, a+e, a+f, b+d, b+e, b+f, c+d, c+e, c+f}
distrib :: Ord a => Set (Set a) -> Set (Set a) -> Set (Set a)
distrib lss rss = flatten $ map (\ rs -> (map (\ ls -> union rs ls) lss)) rss

#if !MIN_VERSION_containers(0,5,11)
cartesianProduct :: (Ord a, Ord b) => Set a -> Set b -> Set (a, b)
cartesianProduct xs ys = flatten $ Set.map (\ x -> Set.map (\ y -> (x, y)) ys) xs
#endif

groupBy :: (Ord a, Ord b) => (a -> b) -> Set a -> Map.Map b (Set a)
groupBy f xs = fold (\ x m -> Map.insertWith union (f x) (singleton x) m) Map.empty xs

powerset :: Ord a => Set a -> Set (Set a)
powerset s
    | s == Set.empty = singleton Set.empty
    | otherwise = Set.map (insert x) pxs `union` pxs
        where (x, xs) = deleteFindMin s
              pxs = powerset xs

partitionM :: (Monad m, Ord a) => (a -> m Bool) -> Set a -> m (Set a, Set a)
partitionM p xs =
    List.foldM f (Set.empty, Set.empty) (toList xs)
    where f (ts, fs) x = p x >>= \ flag -> return $ if flag then (insert x ts, fs) else (ts, insert x fs)

unzip :: (Ord a, Ord b) => Set (a, b) -> (Set a, Set b)
unzip s = Set.fold (\ (l, r) (ls, rs) -> (Set.insert l ls, Set.insert r rs)) (Set.empty, Set.empty) s
