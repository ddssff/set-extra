{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Data.Set.ExtraG 
    ( gFind
    ) where

import Data.Generics hiding (GT)
import Control.Monad.Reader
import Data.Set (Set, fromList)

gFind :: forall a. (Ord a, Typeable a, Data a) => a -> Set a
gFind x = fromList (gFind' x :: [a])

-- | @gFind a@ will extract any elements of type @b@ from
-- @a@'s structure in accordance with the MonadPlus
-- instance, e.g. Maybe Foo will return the first Foo
-- found while [Foo] will return the list of Foos found.
gFind' :: (MonadPlus m, Data a, Typeable b) => a -> m b
gFind' = msum . map return . listify (const True)
