{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.QuadTree.BoundedPoints
    ( BoundedPoints
    , singleton
    , pointList
    , bounds
    , quadTree
    ) where

import Data.Foldable as F
import Data.Traversable as T

import Data.Semigroup
import Data.List.NonEmpty as NonEmpty
import Control.Lens
import Linear
import Linear.Affine

import           Data.QuadTree.Types
import qualified Data.QuadTree.Box as Box
import           Data.QuadTree.Box (Box)
import qualified Data.QuadTree as QT

-- | A list of points and their bounding box.
-- This offers a convenient way to build up the bounding box of a set of points
-- in preparation for building a quad tree of them.
data BoundedPoints x a = BoundedPoints (Box x) (NonEmpty.NonEmpty (Pair (Point V2 x) a))
                       | EmptyPoints
                       deriving (Read, Show, Functor, Foldable, Traversable)

instance (Ord x, Fractional x) => Monoid (BoundedPoints x a) where
    mempty = EmptyPoints
    EmptyPoints `mappend` a = a
    a `mappend` EmptyPoints = a
    BoundedPoints boxA (ptA :| ptsA) `mappend` BoundedPoints boxB (ptB :| ptsB) =
        BoundedPoints (boxA <> boxB) (ptA :| (ptB:ptsA ++ ptsB))

-- | A singleton list
singleton :: Num x => Point V2 x -> a -> BoundedPoints x a
singleton x a = BoundedPoints (Box.singleton x) (Pair x a :| [])

-- | Isomorphism between a bounded list of points and a quad tree
quadTree :: (Ord x, Fractional x)
         => Iso' (BoundedPoints x a) (QT.QuadTree x a)
quadTree = iso to_ from_
  where
    from_ qt
      | [] <- pts   = EmptyPoints
      | p:ps <- pts = BoundedPoints (qt ^. QT.qBox) (p :| ps)
      -- GHC complains about non-exhaustive match above
      | otherwise   = error "quadTree: ghc wut?"
      where pts = QT.toPoints qt
    to_ (BoundedPoints box pts) =
      foldl' (\qt (Pair x a) -> fromJust $ QT.insert x a qt)
        (QT.newWithBB box) (F.toList pts)
      where
        fromJust Nothing = error "quadTree: Impossible"
        fromJust (Just a) = a

-- | Isomorphism between a list of points and a bounded list of points
pointList :: (Ord x, Fractional x)
          => Iso' [Pair (Point V2 x) a] (BoundedPoints x a)
pointList = iso from_ to_
  where
    from_ [] = EmptyPoints
    from_ pts = F.foldMap (\(Pair p a)->singleton p a) pts
    to_ EmptyPoints = []
    to_ (BoundedPoints _ ps) = NonEmpty.toList ps

-- | The bounds of a bounded point list
bounds :: BoundedPoints x a -> Maybe (Box x)
bounds EmptyPoints = Nothing
bounds (BoundedPoints box _) = Just box
