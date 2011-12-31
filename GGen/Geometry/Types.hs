{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies, UndecidableInstances, TemplateHaskell
           , TypeSynonymInstances #-}

module GGen.Geometry.Types ( -- | General
                             pointTol
                           , dirTol
                           , ApproxEq(..)
                           , NonNull(..)
                           , nubPoints
                           , nubPointsWithTol
                           , (=~), (>~), (<~), (/=)
                             -- | Three dimensional geometry
                           , Vec3
                           , NVec3
                           , Point3
                           , Face(..)
                           , translateFace
                           , faceFromVertices
                           -- | Two dimensional geometry
                           , Vec2
                           , NVec2
                           , Point2
                           , ls2Normal
                           -- | n dimensional geometry
                           , Point(..)
                           , sameDir
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
import Data.AffineSpace
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

class ApproxEq a where
        approx :: a -> a -> Bool

instance ApproxEq a => ApproxEq [a] where
        a `approx` b = and $ zipWith approx a b

infix  4  =~, /~, <~, >~

(=~), (/~) :: ApproxEq a => a -> a -> Bool
(=~) = approx
(/~) = (not .) . approx

(>~), (<~) :: (ApproxEq a, Ord a) => a -> a -> Bool
x >~ y = x > y || x =~ y
x <~ y = x < y || x =~ y

-- This might not be a good idea, approximate is a relative term
instance ApproxEq Double where
        a `approx` b = abs (a - b) < 1e-8

instance (ApproxEq v, InnerSpace v, RealFloat (Scalar v)) => ApproxEq (Point v) where
        a `approx` b = distance a b < realToFrac pointTol

-- Three dimensional geometry

-- | Spatial vector (e.g. direction)
type Vec3 = (Double, Double, Double)

instance ApproxEq Vec3 where approx = sameDir

instance Arbitrary (NonNull Vec3) where
        arbitrary = do NonZero a <- arbitrary
                       NonZero b <- arbitrary
                       NonZero c <- arbitrary
                       return $ NonNull (a,b,c)

-- | Unit normalized spatial vector
-- TODO: Enforce with type system?
type NVec3 = Vec3

-- | Spatial point (i.e. location in space)
type Point3 = Point Vec3

-- | Face defined by unit normal and three vertices
data Face = Face { faceNormal :: NVec3
                 , faceVertices :: (Point3,Point3,Point3)
                 } deriving (Show)
                 
translateFace :: Face -> Vec3 -> Face
translateFace face@(Face {faceVertices=(a,b,c)}) v =
        face { faceVertices=(a.+^v, b.+^v, c.+^v) }

faceFromVertices :: (Point3, Point3, Point3) -> Face
faceFromVertices vs@(a,b,c) = let u = b .-. a
                                  v = c .-. a
                              in Face (normalized $ u `cross3` v) vs

instance Arbitrary Face where
        arbitrary = do p <- arbitrary
                       NormalizedV n <- arbitrary
                       return $ Face n p


-- Two dimensional geometry

-- | Spatial vector (e.g. direction)
type Vec2 = (Double, Double)

instance ApproxEq Vec2 where approx = sameDir

instance Arbitrary (NonNull Vec2) where
        arbitrary = do NonZero a <- arbitrary
                       NonZero b <- arbitrary
                       return $ NonNull (a,b)

-- | Unit normalized spatial vector
type NVec2 = Vec2

-- | Spatial point
type Point2 = Point Vec2

-- | Find the normal to a line segment in the given direction. ls2Normal
-- (LineSeg a b) LeftHanded yields a normal pointing to the left as one travels
-- from a to b
ls2Normal :: LineSeg Vec2 -> Hand -> Vec2
ls2Normal l LeftHanded = - ls2Normal l RightHanded
ls2Normal l RightHanded
        | magnitude (x,y) =~ 0    = error "Trying to get normal of zero-magnitude vector"
        | otherwise               = normalized (-y, x)
        where (x,y) = lsDispl l


-- General geometry

-- | A point in an affine-space 
newtype Point v = P v deriving (Show, Eq)

instance AdditiveGroup v => AffineSpace (Point v) where
        type Diff (Point v) = v
        P v1 .-. P v2 = v1 ^-^ v2
        P v1 .+^ v2   = P (v1 ^+^ v2)

instance Ord v => Ord (Point v) where
        P a `compare` P b = a `compare` b

instance (Arbitrary v, Ord v) => Arbitrary (Point v) where
        arbitrary = do v <- arbitrary
                       return (P v)

-- | Eliminate duplicate coincident points
nubPoints :: (InnerSpace v, RealFloat (Scalar v)) => [Point v] -> [Point v]
nubPoints = nubBy coincident

-- | Eliminate duplicate coincident points with some tolerance
nubPointsWithTol :: (InnerSpace v, s ~ Scalar v, AdditiveGroup s, RealFloat s) => s -> [v] -> [v]
nubPointsWithTol tol = nubBy (\x y->magnitude (x,y) < tol)

-- | Are two vectors strictly parallel to within dirTol?
sameDir :: (RealFloat (Scalar v), InnerSpace v) => v -> v -> Bool
sameDir a b = a `parallel` b && a <.> b > 0
{-# SPECIALIZE sameDir :: Vec2 -> Vec2 -> Bool #-}
{-# SPECIALIZE sameDir :: Vec3 -> Vec3 -> Bool #-}

-- | Are two vectors parallel (or antiparallel) to within dirTol?
parallel :: (RealFloat (Scalar v), InnerSpace v) => v -> v -> Bool
parallel a b = 1 - abs (normalized a <.> normalized b) < realToFrac dirTol
{-# SPECIALIZE parallel :: Vec2 -> Vec2 -> Bool #-}
{-# SPECIALIZE parallel :: Vec3 -> Vec3 -> Bool #-}

-- | Are two vectors perpendicular to within dirTol?
perpendicular :: (RealFloat (Scalar v), InnerSpace v) => v -> v -> Bool
perpendicular a b = abs (normalized a <.> normalized b) < realToFrac dirTol
{-# SPECIALIZE perpendicular :: Vec2 -> Vec2 -> Bool #-}
{-# SPECIALIZE perpendicular :: Vec3 -> Vec3 -> Bool #-}

-- | Are two points equal to within pointTol?
coincident :: (RealFloat (Scalar v), InnerSpace v) => Point v -> Point v -> Bool
coincident a b = magnitude (a .-. b) < realToFrac pointTol
{-# SPECIALIZE coincident :: Point Vec2 -> Point Vec2 -> Bool #-}
{-# SPECIALIZE coincident :: Point Vec3 -> Point Vec3 -> Bool #-}

-- | Cuboid defined by two opposite corners
type Box v = (Point v, Point v)

-- | Line segment defined by two terminal points
data LineSeg v = LineSeg { lsA :: Point v
                         , lsB :: Point v
                         } deriving (Show)

instance (InnerSpace v, RealFloat (Scalar v)) => ApproxEq (LineSeg v) where
        u `approx` v  = lsA u `coincident` lsA v && lsB u `coincident` lsB v

-- | Invert the order of line segment termini
lsInvert :: LineSeg v -> LineSeg v
lsInvert (LineSeg a b) = LineSeg b a

-- | Displacement of a line segment
lsDispl :: VectorSpace v => LineSeg v -> v
lsDispl (LineSeg a b) = b .-. a
{-# SPECIALIZE lsDispl :: LineSeg Vec2 -> Vec2 #-}
{-# SPECIALIZE lsDispl :: LineSeg Vec3 -> Vec3 #-}

instance (Arbitrary v, Ord v, VectorSpace v) => Arbitrary (LineSeg v) where
        arbitrary = (liftM2 LineSeg) arbitrary arbitrary

instance (Arbitrary v, Arbitrary (NonNull v), Ord v, Num v, VectorSpace v) => Arbitrary (NonNull (LineSeg v)) where
        arbitrary = do a <- arbitrary
                       NonNull d <- arbitrary
                       return $ NonNull $ LineSeg a (a.+^d)

-- | A contiguous path of line segments
type LineSegPath v = [LineSeg v]

-- | Line defined by point and direction
data Line v = Line { lPoint :: Point v
                   , lDir :: v -- Normalized
                   } deriving (Show)

instance (InnerSpace v, RealFloat (Scalar v)) => ApproxEq (Line v) where
        u `approx` v  = lPoint u `coincident` lPoint v && lDir u `parallel` lDir v

instance (Scalar v ~ s, Floating s, Arbitrary v, Ord v, Num v, InnerSpace v) => Arbitrary (Line v) where
        arbitrary = do point <- arbitrary
                       NormalizedV dir <- arbitrary
                       return $ Line point dir

-- | Ray defined by start point and direction
data Ray v = Ray { rBegin :: Point v
                 , rDir :: v -- Normalized
                 } deriving (Show)

instance (InnerSpace v, RealFloat (Scalar v)) => ApproxEq (Ray v) where
        u `approx` v  = rBegin u `coincident` rBegin v && rDir u `sameDir` rDir v

instance (Scalar v ~ s, Floating s, Arbitrary v, Ord v, Num v, InnerSpace v) => Arbitrary (Ray v) where
        arbitrary = do point <- arbitrary
                       NormalizedV dir <- arbitrary
                       return $ Ray point dir

-- | Plane defined by point and normal
data Plane v = Plane { planeNormal :: v -- Normalized
                     , planePoint :: Point v
                     } deriving (Show, Eq)

instance (InnerSpace v, RealFloat (Scalar v)) => ApproxEq (Plane v) where
        u `approx` v  = planeNormal u `parallel` planeNormal v && planePoint u `coincident` planePoint v

-- | Project a vector onto a plane
projInPlane :: (Num v, InnerSpace v) => Plane v -> v -> v
projInPlane plane x = x - n ^* (n <.> x)
                      where n = planeNormal plane

instance (Arbitrary v, Ord v, Num v, InnerSpace v, Floating (Scalar v)) => Arbitrary (Plane v) where
        arbitrary = do point <- arbitrary
                       NormalizedV normal <- arbitrary
                       return $ Plane {planeNormal=normal, planePoint=point}

-- | Closed polygon defined by a series of connected points
-- The point list should not be closed (i.e. head p /= last p)
newtype Polygon v = Polygon [Point v] deriving (Show, Eq)

invertPolygon :: Polygon v -> Polygon v
invertPolygon (Polygon points) = Polygon (reverse points)

instance (InnerSpace v, RealFloat (Scalar v)) => ApproxEq (Polygon v) where
        (Polygon u) `approx` (Polygon v)  = and $ zipWith coincident u v

-- TODO: Handle n-gons
instance (s ~ Scalar v, Floating s, Arbitrary v, Arbitrary (NonNull v), InnerSpace v, Ord v, Num v) => Arbitrary (Polygon v) where
        arbitrary = do a <- arbitrary
                       NonNull b <- arbitrary
                       NonNull c <- arbitrary
                       let displs = (b:c:[])
                           points = scanl (.+^) a displs
                       return $ Polygon points

-- | Right- or left-handedness
data Hand = LeftHanded | RightHanded deriving (Show, Eq)

-- | A polygon with points in clockwise order. The associated handedness
-- reflects on which side of an edge the normal will be found. Since polygon
-- points are given in clockwise order, an oriented polygon with right-handed
-- orientation will have its normal facing inward. For polygons represented
-- printed areas, the normal points away from the filled body.
type OrientedPolygon v = (Polygon v, Hand)


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
prop_invert_displacement :: LineSeg Vec3 -> Bool
prop_invert_displacement l = (lsDispl $ lsInvert l) == (negateV $ lsDispl l)

-- Properties for perpendicular
prop_perpendicular_perp :: NonZero Vec3 -> NonZero Vec3 -> Bool
prop_perpendicular_perp (NonZero u) (NonZero v) = perpendicular u (u `cross3` v)

prop_perpendicular_par :: Vec3 -> Double -> Bool
prop_perpendicular_par u a = not $ perpendicular u (u ^* a)

runTests = $quickCheckAll

