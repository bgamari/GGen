{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies, UndecidableInstances, TemplateHaskell
           , TypeSynonymInstances #-}

module GGen.Geometry.Types ( -- | General
                             pointTol
                           , dirTol
                           , ApproxEq(..)
                           , NonNull(..)
                           , nubPoints
                           , nubPointsWithTol
                             -- | Three dimensional geometry
                           , Vec
                           , NVec
                           , Point
                           , Face(..)
                           , translateFace
                           , faceFromVertices
                           -- | Two dimensional geometry
                           , Vec2
                           , NVec2
                           , Point2
                           , ls2Normal
                           -- | n dimensional geometry
                           , parallel
                           , perpendicular
                           , coincident
                           , Box
                           , LineSeg(..)
                           , lsInvert
                           , lsDispl
                           , LineSegPath
                           , Line(..)
                           , Ray(..)
                           , Plane(..)
                           , projInPlane
                           , Hand(..)
                           , Polygon(..)
                           , OrientedPolygon
                           -- | Polytope intersection
                           , Intersection(..)
                           , isIntersect
                           , isNull
                           , isDegenerate
                           , mapIntersection
                           , mapIntersectionDropDegen
                             -- | Tests
                           , GGen.Geometry.Types.runTests
                           ) where

import Data.VectorSpace
import Data.Cross

import Test.QuickCheck
import Data.VectorSpace.QuickCheck
import Control.Monad (liftM, liftM2)
import Data.Maybe (mapMaybe)
import Data.List (nubBy)

import Test.QuickCheck.All
import Test.QuickCheck.Property

-- | The maximum distance between identical points
pointTol = 1e-8

-- | The maximum deviation from one in a dot product to consider vectors parallel
dirTol = 1e-8

-- | For tagging values that can't have 0 measure when specifying QuickCheck Arbitrarys
newtype NonNull a = NonNull a deriving Show
--instance (b ~ NonZero a, Arbitrary b) => Arbitrary (NonNull a) where
--        arbitrary = do NonZero a <- arbitrary
--                       return a

class ApproxEq a where
        approx :: a -> a -> Bool

instance ApproxEq a => ApproxEq [a] where
        a `approx` b = and $ zipWith approx a b

-- Three dimensional geometry

-- | Spatial vector (e.g. direction)
type Vec = (Double, Double, Double)

instance ApproxEq Vec where approx = coincident

-- | Unit normalized spatial vector
-- TODO: Enforce with type system?
type NVec = Vec

-- | Spatial point (i.e. location in space)
type Point = Vec

-- | Face defined by normal and three vertices
data Face = Face { faceNormal :: Point
                 , faceVertices :: (Point,Point,Point)
                 } deriving (Show, Eq)
                 
translateFace :: Face -> Vec -> Face
translateFace face@(Face {faceVertices=(a,b,c)}) v =
        face { faceVertices=(a+v, b+v, c+v) }

faceFromVertices :: (Point, Point, Point) -> Face
faceFromVertices vs@(v0,v1,v2) = let u = v1 ^-^ v0
                                     v = v2 ^-^ v1
                                 in Face (normalized $ u `cross3` v) vs

instance Arbitrary Face where
        arbitrary = do NonZero vs <- arbitrary
                       return $ faceFromVertices vs


-- Two dimensional geometry

-- | Spatial vector (e.g. direction)
type Vec2 = (Double, Double)

instance ApproxEq Vec2 where approx = coincident

-- | Unit normalized spatial vector
type NVec2 = Vec2

-- | Spatial point
type Point2 = Vec2

-- | Find the normal to a line segment in the given direction. ls2Normal
-- (LineSeg a b) LeftHanded yields a normal pointing to the left as one travels
-- from a to b
ls2Normal :: LineSeg Point2 -> Hand -> Vec2
ls2Normal l RightHanded
        | magnitude (x,y) < 1e-8  = error "Trying to get normal of zero-magnitude vector"
        | otherwise               = normalized (-y, x)
        where (x,y) = lsDispl l
ls2Normal l LeftHanded = - ls2Normal l RightHanded


-- General geometry

-- | Eliminate duplicate coincident points
nubPoints :: (InnerSpace p, RealFloat (Scalar p)) => [p] -> [p]
nubPoints = nubBy coincident

-- | Eliminate duplicate coincident points with some tolerance
nubPointsWithTol :: (InnerSpace p, s ~ Scalar p, AdditiveGroup s, RealFloat s) => s -> [p] -> [p]
nubPointsWithTol tol = nubBy (\x y->magnitude (x,y) < tol)

-- | Are two vectors strictly parallel to within dirTol?
sameDir :: (RealFloat (Scalar p), InnerSpace p) => p -> p -> Bool
sameDir a b = 0 <= dot && dot < realToFrac dirTol
        where dot = 1 - normalized a <.> normalized b

-- | Are two vectors parallel (or antiparallel) to within dirTol?
parallel :: (RealFloat (Scalar p), InnerSpace p) => p -> p -> Bool
parallel a b = 1 - abs (normalized a <.> normalized b) < realToFrac dirTol

-- | Are two vectors perpendicular to within dirTol?
perpendicular :: (RealFloat (Scalar p), InnerSpace p) => p -> p -> Bool
perpendicular a b = abs (normalized a <.> normalized b) < realToFrac dirTol

-- | Do two vectors identify the same point to within pointTol?
coincident :: (RealFloat (Scalar p), InnerSpace p) => p -> p -> Bool
coincident a b = magnitude (a ^-^ b) < realToFrac pointTol

-- | Cuboid defined by two opposite corners
type Box p = (p, p)

-- | Line segment defined by two terminal points
data LineSeg p = LineSeg { lsA :: p
                         , lsB :: p
                         } deriving (Show)

instance (InnerSpace p, RealFloat (Scalar p)) => ApproxEq (LineSeg p) where
        u `approx` v  = lsA u `coincident` lsA v && lsB u `coincident` lsB v

-- | Invert the order of line segment termini
lsInvert :: LineSeg p -> LineSeg p
lsInvert (LineSeg a b) = LineSeg b a

-- | Displacement of a line segment
lsDispl :: VectorSpace p => LineSeg p -> p
lsDispl (LineSeg a b) = b ^-^ a

instance (Arbitrary p, VectorSpace p) => Arbitrary (LineSeg p) where
        arbitrary = (liftM2 LineSeg) arbitrary arbitrary

instance (Arbitrary p, Ord p, Num p, VectorSpace p) => Arbitrary (NonNull (LineSeg p)) where
        arbitrary = do a <- arbitrary
                       NonZero d <- arbitrary
                       return $ NonNull $ LineSeg a (a+d)

-- | A contiguous path of line segments
type LineSegPath p = [LineSeg p]

-- | Line defined by point and direction
data Line p = Line { lPoint :: p
                   , lDir :: p -- Normalized
                   } deriving (Show)

instance (InnerSpace p, RealFloat (Scalar p)) => ApproxEq (Line p) where
        u `approx` v  = lPoint u `coincident` lPoint v && lDir u `parallel` lDir v

instance (Scalar p ~ s, Floating s, Arbitrary p, Ord p, Num p, InnerSpace p) => Arbitrary (Line p) where
        arbitrary = do point <- arbitrary
                       NormalizedV dir <- arbitrary
                       return $ Line point dir

-- | Ray defined by start point and direction
data Ray p = Ray { rBegin :: p
                 , rDir :: p -- Normalized
                 } deriving (Show)

instance (InnerSpace p, RealFloat (Scalar p)) => ApproxEq (Ray p) where
        u `approx` v  = rBegin u `coincident` rBegin v && rDir u `sameDir` rDir v

instance (Scalar p ~ s, Floating s, Arbitrary p, Ord p, Num p, InnerSpace p) => Arbitrary (Ray p) where
        arbitrary = do point <- arbitrary
                       NormalizedV dir <- arbitrary
                       return $ Ray point dir

-- | Plane defined by point and normal
data Plane p = Plane { planeNormal :: p -- Normalized
                     , planePoint :: p
                     } deriving (Show, Eq)

instance (InnerSpace p, RealFloat (Scalar p)) => ApproxEq (Plane p) where
        u `approx` v  = planeNormal u `parallel` planeNormal v && planePoint u `coincident` planePoint v

-- | Project a vector onto a plane
projInPlane :: (Num p, InnerSpace p) => Plane p -> p -> p
projInPlane plane x = x - n ^* (n <.> x)
                      where n = planeNormal plane

instance (Arbitrary p, Ord p, Num p, InnerSpace p, Floating (Scalar p)) => Arbitrary (Plane p) where
        arbitrary = do point <- arbitrary
                       NormalizedV normal <- arbitrary
                       return $ Plane {planeNormal=normal, planePoint=point}

-- | Closed polygon defined by a series of connected points
-- The point list should not be closed (i.e. head p /= last p)
newtype Polygon p = Polygon [p] deriving (Show, Eq)

instance (InnerSpace p, RealFloat (Scalar p)) => ApproxEq (Polygon p) where
        (Polygon u) `approx` (Polygon v)  = and $ zipWith coincident u v

instance (s ~ Scalar p, Floating s, Arbitrary p, InnerSpace p, Ord p, Num p) => Arbitrary (Polygon p) where
        arbitrary = do let unNonZero (NonZero a) = a
                       points <- (liftM $ map unNonZero) arbitrary
                       return $ Polygon $ points++[head points]

-- | Right- or left-handedness
data Hand = LeftHanded | RightHanded deriving (Show, Eq)

-- | A polygon with points in clockwise order. The associated handedness
-- reflects on which side of an edge the normal will be found. Since polygon
-- points are given in clockwise order, an oriented polygon with right-handed
-- orientation will have its normal facing inward. For polygons represented
-- printed areas, the normal points away from the filled body.
type OrientedPolygon p = (Polygon p, Hand)


-- Polytope intersection

-- | Represents the possible intersection between two bodies
data Intersection a = IIntersect a  -- | Intersection of type a
                    | INull         -- | No intersection
                    | IDegenerate   -- | Degenerate case (e.g. intersection of higher dimension than a)
                    deriving (Eq, Show)

isIntersect, isNull, isDegenerate :: Intersection a -> Bool
isIntersect (IIntersect _) = True
isIntersect _              = False
isNull (INull) = True
isNull _       = False
isDegenerate (IDegenerate) = True
isDegenerate _             = False

instance Monad Intersection where
        (IIntersect a) >>= f    = f a
        INull >>= _             = INull
        IDegenerate >>= _       = IDegenerate
        return a                = IIntersect a

liftMaybe :: Maybe a -> Intersection a
liftMaybe (Just a) = IIntersect a
liftMaybe Nothing = INull

mapIntersection :: (a -> Intersection b) -> [a] -> [b]
mapIntersection f = mapMaybe g
                    where g i = case f i of
                                    IIntersect a   -> Just a
                                    INull          -> Nothing
                                    IDegenerate    -> error "Degeneracy"

mapIntersectionDropDegen :: (a -> Intersection b) -> [a] -> [b]
mapIntersectionDropDegen f = mapMaybe g
                    where g i = case f i of
                                    IIntersect a   -> Just a
                                    INull          -> Nothing
                                    IDegenerate    -> Nothing

-- QuickCheck properties

-- Properties for lsInvertDispl
prop_invert_displacement :: LineSeg Point -> Bool
prop_invert_displacement l = (lsDispl $ lsInvert l) == (negateV $ lsDispl l)

-- Properties for perpendicular
prop_perpendicular_perp :: NonZero Vec -> NonZero Vec -> Bool
prop_perpendicular_perp (NonZero u) (NonZero v) = perpendicular u (u `cross3` v)

prop_perpendicular_par :: Vec -> Double -> Bool
prop_perpendicular_par u a = not $ perpendicular u (u ^* a)

runTests = $quickCheckAll

