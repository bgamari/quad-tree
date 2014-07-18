{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

-- Implementation note: Points sitting on edges are biased to the NE quadrant

module Data.QuadTree
    ( Pair(..)
      -- * Quad tree type
    , QuadTree
      -- * Creating and populating quad trees
    , newWithBB
    , insert
    , insertWith
      -- * Querying quad trees
    , boundingBox
    , lookup
    , size
    , toPoints
    ) where

import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)
import Data.Foldable as F
import Data.Traversable as T
import Control.Applicative
import Control.Monad (join)

import Control.Lens hiding (children)
import Linear
import Linear.Affine

import           Data.QuadTree.Types
import qualified Data.QuadTree.Box as Box
import           Data.QuadTree.Box (Box (Box))

-- | Either positive or negative
data Sign = Plus | Minus
          deriving (Read, Show, Ord, Eq)
makePrisms ''Sign

-- | Multiply by a sign
multBy :: Num a => a -> Sign -> a
multBy a Plus  =  a
multBy a Minus = -a

data Quadrants a = Quadrants { _qNE, _qNW, _qSE, _qSW :: !a }
                 deriving (Read, Show, Ord, Eq, Functor, Foldable, Traversable)
makeLenses ''Quadrants

instance Applicative Quadrants where
    pure a = Quadrants a a a a
    Quadrants ne nw se sw <*> Quadrants ne' nw' se' sw' =
        Quadrants (ne ne') (nw nw') (se se') (sw sw')

-- | The signs of the quadrants
quadrantSigns :: Quadrants (V2 Sign)
quadrantSigns = Quadrants { _qNE = V2 Plus  Plus  , _qNW = V2 Minus Plus
                          , _qSE = V2 Plus  Minus , _qSW = V2 Minus Minus
                          }

-- | A box representing the given quadrant of the given box
quadrantOf :: Fractional a => V2 Sign -> Box a -> Box a
quadrantOf quadrant (Box c h) =
    Box (c .+^ offset) half
  where
    offset = multBy <$> half <*> quadrant
    half = h ^/ 2

-- | Boxes representing the quadrants of a box
quadrantsOf :: (Fractional a) => Box a -> Quadrants (Box a)
quadrantsOf box = quadrantOf <$> quadrantSigns <*> pure box


data QuadTree x a = Node { _qChildren :: !(Quadrants (QuadTree x a))
                         , _qBox :: !(Box x)
                         }
                  | Leaf { _qPoints :: [Pair (Point V2 x) a]
                         , _qBox :: !(Box x)
                         }
                  deriving (Read, Show)
makeLenses ''QuadTree

instance Functor (QuadTree x) where
    fmap f (Node children box) = Node (fmap (fmap f) children) box
    fmap f (Leaf points box) =
        Leaf (map (\(Pair x a)->Pair x (f a)) points) box

-- | Create a new quadtree covering the given bounding-box
newWithBB :: Box x -> QuadTree x a
newWithBB bb = Leaf [] bb

-- | Find a point in a quadtree
lookup :: (Fractional x, Ord x, Eq x)
       => Point V2 x -> QuadTree x a -> Maybe a
lookup x = go
  where
    go (Leaf points _) = lookupPair x points
    go (Node quads box)
      | not $ box `Box.contains` x = Nothing
      | otherwise = join $ withQuadrantFor x box $ \l->
          lookup x (quads ^. l)

-- | Evaluate the given function with a lens pointing to the quadrant
-- containing the given point (or @Nothing@ if the point is not contained
-- within the box
withQuadrantFor
    :: (Ord x, Fractional x)
    => Point V2 x
    -> Box x
    -> ((forall a. Lens' (Quadrants a) a) -> b)
    -> Maybe b
withQuadrantFor p@(P (V2 x y)) box f
    | not $ box `Box.contains` p      = Nothing
    | otherwise =
         if x < cx
           then if y < cy
                  then Just (f qSW)
                  else Just (f qNW)
           else if y < cy
                  then Just (f qSE)
                  else Just (f qNE)
  where
    P (V2 cx cy) = box ^. Box.center
{-# INLINE withQuadrantFor #-}

-- | Subdivide the leaves of a given quadtree
subdivide :: forall x a. (Ord x, Fractional x)
          => QuadTree x a -> QuadTree x a
subdivide (Leaf pts box) =
    Node (Leaf <$> sortPoints pts <*> boxes) box
  where
    insert :: (Ord x, Num x)
           => Quadrants [Pair (Point V2 x) a]
           -> Pair (Point V2 x) a
           -> Quadrants [Pair (Point V2 x) a]
    insert quads p@(Pair x _) =
        fromMaybe (error "subdivide: Uh oh.")
        $ withQuadrantFor x box $ \l->quads & l %~ (p:)
    sortPoints :: [Pair (Point V2 x) a] -> Quadrants [Pair (Point V2 x) a]
    sortPoints = foldl' insert (pure [])
    boxes = quadrantsOf box

subdivide (Node quadrants box) =
    Node (fmap subdivide quadrants) box

maxLeafSize :: Int
maxLeafSize = 10

-- | The number of elements in a quadtree
size :: QuadTree x a -> Int
size (Leaf pts _) = length pts
size (Node quads _) = F.sum $ fmap size quads

-- | Subdivide a leaf if larger than @maxLeafSize@
subdivideIfNeeded :: (Ord x, Fractional x)
                  => QuadTree x a -> QuadTree x a
subdivideIfNeeded qt@(Leaf {})
  | size qt > maxLeafSize = subdivide qt
subdivideIfNeeded qt = qt

extract :: Eq a => a -> [Pair a b] -> Maybe (b, [Pair a b])
extract a xs = go [] xs
  where
    go prefix (Pair x y:rest)
      | a == x    = Just (y, prefix++rest)
      | otherwise = go (Pair x y : prefix) rest
    go _ [] = Nothing

-- | Insert a point into a quadtree combining with existing point if necessary
insertWith :: (Ord x, Fractional x)
           => (a -> a -> a)    -- ^ Combining function
           -> Point V2 x       -- ^ Point at which to insert
           -> a                -- ^ Value to insert
           -> QuadTree x a     -- ^ Quad tree to insert into
           -> Maybe (QuadTree x a) -- ^ @Nothing@ if point not covered by tree
insertWith combine x a qt = go qt
  where
    go (Leaf children box)
      | box `Box.contains` x =
          let children' =
                case extract x children of
                  Just (y, children') -> Pair x (combine y a) : children'
                  Nothing             -> Pair x a : children
          in Just $ subdivideIfNeeded $ Leaf children' box
      | otherwise = Nothing
    go (Node quads box) =
      let fromJust = fromMaybe (error "insert: Uh oh")
          quads' = withQuadrantFor x box
                     (\l -> quads & l %~ fromJust . insertWith combine x a)
      in fmap (\q->Node q box) quads'

-- | Insert a point into a quadtree
insert :: (Ord x, Fractional x)
       => Point V2 x       -- ^ Point at which to insert
       -> a                -- ^ Value to insert
       -> QuadTree x a     -- ^ Quad tree to insert into
       -> Maybe (QuadTree x a) -- ^ @Nothing@ if point not covered by tree
insert x a qt = insertWith (const id) x a qt

-- | Return the list of points contained in the quad tree
toPoints :: QuadTree x a -> [Pair (Point V2 x) a]
toPoints (Leaf pts _) = pts
toPoints (Node quads _) = F.concat $ F.toList $ fmap toPoints quads

-- | The bounding box of a quad tree
boundingBox :: QuadTree x a -> Box x
boundingBox qt = qt ^. qBox
