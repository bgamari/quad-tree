{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.QuadTree.Types
    ( Pair (..)
    , pairTuple
    , lookupPair
    ) where

import Data.Foldable
import Data.Traversable
    
import Control.Lens

-- | A strict pair
data Pair a b = Pair !a !b
              deriving (Read, Show, Functor, Foldable, Traversable)

pairTuple :: Iso' (Pair a b) (a, b)
pairTuple = iso (\(Pair a b) -> (a, b)) (\(a, b) -> Pair a b)

lookupPair :: Eq a => a -> [Pair a b] -> Maybe b
lookupPair a xs = go xs
  where
    go (Pair x y:rest)
      | a == x    = Just y
      | otherwise = go rest
    go [] = Nothing
