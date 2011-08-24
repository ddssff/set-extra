module Data.Set.Extra
    ( module Data.Set
    , catMaybes 
    , flatten
    , concatMap
    , mapM
    , filterM
    , concatMapM
    , any
    , all
    , distrib
    , or
    , and
    , ss
    , ssMapM
    , toSS
    , fromSS
    , cartesianProduct
    -- , anyM
    , groupBy
    , partitionM
    ) where

import qualified Control.Monad as List (mapM, filterM, foldM)
import qualified Data.Map as Map
import Data.Set
import qualified Data.List as List
--import qualified Data.Maybe
import Prelude hiding (mapM, all, any, map, filter, null, concatMap, and, or)
--import qualified Prelude

mapM :: (Monad m, Ord b) => (a -> m b) -> Set a -> m (Set b)
mapM f s = List.mapM f (toList s) >>= return . fromList

filterM :: (Ord a, Monad m) => (a -> m Bool) -> Set a -> m (Set a)
filterM p s = List.filterM p (toList s) >>= return . fromList

catMaybes :: Ord a => Set (Maybe a) -> Set a
catMaybes sm = fold (\ mx s -> maybe s (`insert` s) mx) empty sm

flatten :: Ord a => Set (Set a) -> Set a
flatten ss' = fold union empty ss'
--flatten = unions . toList

concatMap :: (Ord a, Ord b) => (a -> Set b) -> Set a -> Set b
concatMap f s = flatten (Data.Set.map f s)

concatMapM :: (Monad m, Ord a, Ord b) => (a -> m (Set b)) -> Set a -> m (Set b)
concatMapM f s = mapM f s >>= return . flatten

any :: Ord a => (a -> Bool) -> Set a -> Bool
any f s = not . null . filter id . map f $ s

{-
anyM :: Monad m => (a -> m (Maybe Bool)) -> Set a -> m (Maybe Bool)
anyM p s =
    List.mapM p (toList s) >>= return . Data.Maybe.catMaybes >>= return . chk
    where chk [] = Nothing
          chk ys = Just (Prelude.or ys)
-}

all :: Ord a => (a -> Bool) -> Set a -> Bool
all f s = not . null . filter not . map f $ s

or :: Set Bool -> Bool
or = any id

and :: Set Bool -> Bool
and = all id

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

distrib :: Ord a => Set (Set a) -> Set (Set a) -> Set (Set a)
distrib lss rss = flatten $ map (\ rs -> (map (\ ls -> union rs ls) lss)) rss

cartesianProduct :: (Ord a, Ord b) => Set a -> Set b -> Set (a, b)
cartesianProduct xs ys = flatten $ Data.Set.map (\ x -> Data.Set.map (\ y -> (x, y)) ys) xs

groupBy :: (Ord a, Ord b) => (a -> b) -> Set a -> Map.Map b (Set a)
groupBy f xs = fold (\ x m -> Map.insertWith union (f x) (singleton x) m) Map.empty xs

partitionM :: (Monad m, Ord a) => (a -> m Bool) -> Set a -> m (Set a, Set a)
partitionM p xs =
    List.foldM f (empty, empty) (toList xs)
    where f (ts, fs) x = p x >>= \ flag -> return $ if flag then (insert x ts, fs) else (ts, insert x fs)
