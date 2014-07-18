{-# LANGUAGE TemplateHaskell #-}

module Data.QuadTree.Box
    ( Box (..)
    , center
    , halfDim
    , singleton
    , corners
    , contains
    ) where
    
import Control.Applicative

import Data.Semigroup
import Control.Lens hiding (contains)
import Linear
import Linear.Affine

-- | An axis-aligned box closed on negative edges and open on positive
-- edges
data Box a = Box { _center :: !(Point V2 a)
                 , _halfDim :: !(V2 a)
                 }
           deriving (Read, Show, Eq)
makeLenses ''Box

-- | Construct a box from its opposing corners.
-- The isomorphism only holds up to flipping of the two points.
-- If @(a,b) == box ^. from corners@, then @a ^. _x <= b ^. _x@
-- and the same for @_y@.
corners :: (Ord a, Fractional a) => Iso' (Box a) (Point V2 a, Point V2 a)
corners = iso to_ from_
  where
    to_ (Box c h) = (c .-^ h, c .+^ h)
    from_ (P (V2 ax ay), P (V2 bx by)) =
      let f a b = if a > b then (b, a - b) else (a, b - a)
          (x0, dx) = f ax bx
          (y0, dy) = f ay by
          half = V2 (dx / 2) (dy / 2)
          center_ = P (V2 x0 y0) .+^ half
      in Box center_ half

instance (Ord a, Fractional a) => Semigroup (Box a) where
    a <> b = (minP, maxP) ^. from corners
      where
        (aMin, aMax) = a ^. corners
        (bMin, bMax) = b ^. corners
        minP = min <$> aMin <*> bMin
        maxP = max <$> aMax <*> bMax

-- | A box containing a single point
singleton :: Num a => Point V2 a -> Box a
singleton v = Box v zero

-- | Test whether a a box contains a point
contains :: (Num a, Ord a) => Box a -> Point V2 a -> Bool
contains (Box (P (V2 cx cy)) (V2 hx hy)) (P (V2 x y))
    | x < cx - hx || x >= cx + hx = False
    | y < cy - hy || y >= cy + hy = False
    | otherwise = True
