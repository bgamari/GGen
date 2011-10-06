{-# LANGUAGE FlexibleInstances, TypeFamilies, UndecidableInstances, TemplateHaskell #-}

module GGen.Geometry.Types ( pointTol
                           , dirTol
                           , Vec
                           , NVec
                           , parallel
                           , perpendicular
                           , Point
                           , coincident
                           , Box
                           , LineSeg(..)
                           , LineSegPath
                           , Line(..)
                           , Plane(..)
                           , projInPlane
                           , Face(..)
                           , translateFace
                           , faceFromVertices
                           , Polygon
                           , Ray(..)
                           , Intersection(..)
                           , mapIntersection
                           , NonNull(..)
                           , GGen.Geometry.Types.runTests
                           ) where

import Data.VectorSpace
import Data.Cross

import Test.QuickCheck
import Data.VectorSpace.QuickCheck
import Control.Monad (liftM, liftM2)
import Data.Maybe (mapMaybe)

import Test.QuickCheck.All
import Test.QuickCheck.Property

-- | The maximum distance between identical points
pointTol = 1e-2 :: Double

-- | The maximum deviation from one in a dot product to consider vectors parallel
dirTol = 1e-3 :: Double


-- | Spatial vector (e.g. direction)
type Vec = (Double, Double, Double)

-- | Unit normalized spatial vector
-- TODO: Enforce with type system?
type NVec = Vec

-- | Are two vectors parallel (or antiparallel) to within dirTol?
parallel :: Vec -> Vec -> Bool
parallel a b = 1 - abs (normalized a <.> normalized b) < dirTol

-- | Are two vectors perpendicular to within dirTol?
perpendicular :: Vec -> Vec -> Bool
perpendicular a b = abs (normalized a <.> normalized b) < dirTol

-- | Spatial point
type Point = Vec

-- | Cuboid defined by two opposite corner
type Box = (Point, Point)

-- | Are two points the same to within pointTol?
coincident :: Point -> Point -> Bool
coincident a b = magnitude (a ^-^ b) < pointTol

-- | Line segment defined by two terminal points
data LineSeg = LineSeg { lsBegin :: Point
                       , lsEnd :: Point
                       } deriving (Show, Eq)

instance Arbitrary LineSeg where
        arbitrary = (liftM2 LineSeg) arbitrary arbitrary

newtype NonNull a = NonNull a deriving Show
--instance (b ~ NonZero a, Arbitrary b) => Arbitrary (NonNull a) where
--        arbitrary = do NonZero a <- arbitrary
--                       return a
instance Arbitrary (NonNull LineSeg) where
        arbitrary = do a <- arbitrary
                       NonZero d <- arbitrary
                       return $ NonNull $ LineSeg a (a+d)

-- | A contiguous path of line segments
type LineSegPath = [LineSeg]

-- | Line defined by point and direction
data Line = Line { lPoint :: Point
                 , lDir :: NVec
                 } deriving (Show, Eq)

instance Arbitrary Line where
        arbitrary = do point <- arbitrary
                       NormalizedV dir <- arbitrary
                       return $ Line point dir

-- | Ray defined by start point and direction
data Ray = Ray { rBegin :: Point
               , rDir :: NVec
               } deriving (Show, Eq)

instance Arbitrary Ray where
        arbitrary = do point <- arbitrary
                       NormalizedV dir <- arbitrary
                       return $ Ray point dir

-- | Plane defined by point and normal
data Plane = Plane { planeNormal :: NVec
                   , planePoint :: Point
                   } deriving (Show, Eq)

-- | Project a vector onto a plane
projInPlane plane x = x - n ^* (n <.> x)
                      where n = planeNormal plane

instance Arbitrary Plane where
        arbitrary = do point <- arbitrary
                       NormalizedV normal <- arbitrary
                       return $ Plane {planeNormal=normal, planePoint=point}

-- | Face defined by normal and three vertices
data Face = Face { faceNormal :: Point
                 , faceVertices :: (Point,Point,Point)
                 } deriving (Show, Eq)
                 
translateFace :: Face -> Vec -> Face
translateFace face@(Face {faceVertices=(a,b,c)}) v =
        face { faceVertices=(a+v, b+v, c+v) }
faceFromVertices vs@(v0,v1,v2) = let u = v1 ^-^ v0
                                     v = v2 ^-^ v1
                                 in Face (normalized $ u `cross3` v) vs

instance Arbitrary Face where
        arbitrary = do NonZero vs <- arbitrary
                       return $ faceFromVertices vs

-- | Closed polygon defined by a series of connected points
type Polygon = [Point]

-- | Represents the possible intersection between two bodies
data Intersection a = IIntersect a  -- | Intersection of type a
                    | INull         -- | No intersection
                    | IDegenerate   -- | Degenerate case (e.g. intersection of higher dimension than a)
                    deriving (Eq, Show)

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

-- QuickCheck properties

-- Properties for perpendicular
prop_perpendicular_perp :: NonZero Vec -> NonZero Vec -> Bool
prop_perpendicular_perp (NonZero u) (NonZero v) = perpendicular u (u `cross3` v)

prop_perpendicular_par :: Vec -> Double -> Bool
prop_perpendicular_par u a = not $ perpendicular u (u ^* a)

runTests = $quickCheckAll

