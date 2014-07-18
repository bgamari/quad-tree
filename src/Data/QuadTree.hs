{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Implementation note: Points sitting on edges are biased to the NE quadrant

module Data.QuadTree
    ( Pair(..)
    , Sign(..)
    , Quadrants(..)
    , qNE, qNW, qSE, qSW
    , quadrantSigns
      -- * Quad tree
    , QuadTree
    , qBox
    , newWithBB
    , lookupNearest
    , size
    , insert
    , toPoints
    ) where
    
import Data.Maybe (fromMaybe)
import Data.Foldable as F
import Data.Traversable as T
import Control.Applicative

import Control.Lens hiding (children)
import Linear
import Linear.Affine

import           Data.QuadTree.Types
import qualified Data.QuadTree.Box as Box
import           Data.QuadTree.Box (Box (Box))


data Sign = Plus | Minus
          deriving (Read, Show, Ord, Eq)
makePrisms ''Sign

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

-- | Find the nearest point in a quadtree
lookupNearest :: (Num x, Ord x) => Point V2 x -> QuadTree x a -> Maybe a
lookupNearest x n@(Node {})
    | not $ (n ^. qBox) `Box.contains` x = Nothing
    | otherwise = undefined

-- | Apply a function to the quadrant containing the given point
mapContainingQuadrant
     :: (Ord x, Fractional x)
     => (a -> a)            -- ^ The function to apply
     -> Point V2 x          -- ^ The contained point
     -> Box x               -- ^ The box covered by the quadrants
     -> Quadrants a         -- ^ The quadrants themselves
     -> Maybe (Quadrants a) -- ^ Nothing if point not in covered box
mapContainingQuadrant f x box quads
    | (boxes ^. qNE) `Box.contains` x = Just (quads & qNE %~ f)
    | (boxes ^. qNW) `Box.contains` x = Just (quads & qNW %~ f)
    | (boxes ^. qSE) `Box.contains` x = Just (quads & qSE %~ f)
    | (boxes ^. qSW) `Box.contains` x = Just (quads & qSW %~ f)
    | otherwise                      = Nothing
  where
    boxes = quadrantsOf box

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
        $ mapContainingQuadrant (p:) x box quads
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

subdivideIfNeeded :: (Ord x, Fractional x)
                  => QuadTree x a -> QuadTree x a
subdivideIfNeeded qt@(Leaf {})
  | size qt > maxLeafSize = subdivide qt
subdivideIfNeeded qt = qt

-- | Insert a point into a quadtree
insert :: (Ord x, Fractional x)
       => Point V2 x -> a -> QuadTree x a -> Maybe (QuadTree x a)
insert x a (Leaf children box)
  | box `Box.contains` x = Just $ subdivideIfNeeded $ Leaf (Pair x a:children) box
  | otherwise = Nothing
insert x a (Node quads box) =
  let quads' = mapContainingQuadrant
                 (fromMaybe (error "insert: Uh oh") . insert x a) x box quads
  in fmap (\q->Node q box) quads'

-- | Return the list of points contained in the quad tree
toPoints :: QuadTree x a -> [Pair (Point V2 x) a]
toPoints (Leaf pts _) = pts
toPoints (Node quads _) = F.concat $ F.toList $ fmap toPoints quads
