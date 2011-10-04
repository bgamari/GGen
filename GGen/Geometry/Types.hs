module GGen.Geometry.Types ( pointTol
                           , dirTol
                           , Vec
                           , sameDir
                           , Point
                           , samePoint
                           , LineSeg(..)
                           , LineSegPath
                           , Line(..)
                           , Plane(..)
                           , Face(..)
                           , Polygon
                           , Ray(..)
                           ) where

import Data.VectorSpace
import Data.Cross

import Test.QuickCheck
import Data.VectorSpace.QuickCheck
import Control.Monad (liftM, liftM2)

-- | The maximum distance between identical points
pointTol = 1e-2 :: Double

-- | The maximum deviation from one in a dot product to consider vectors parallel
dirTol = 1e-3 :: Double


-- | Spatial vector
type Vec = (Double, Double, Double)

-- | Are two vectors parallel to within dirTol
sameDir :: Vec -> Vec -> Bool
sameDir a b = abs (normalized a <.> normalized b) - 1 < dirTol

-- | Spatial point
type Point = Vec

-- | Are two points the same to within pointTol?
samePoint :: Point -> Point -> Bool
samePoint a b = magnitude (a ^-^ b) < pointTol

-- | Line segment defined by two terminal points
data LineSeg = LineSeg { lsBegin :: Point
                       , lsEnd :: Point
                       } deriving (Show, Eq)

instance Arbitrary LineSeg where
        arbitrary = (liftM2 LineSeg) arbitrary arbitrary

-- | A contiguous path of line segments
type LineSegPath = [LineSeg]

-- | Line defined by point and direction
data Line = Line { lPoint :: Point
                 , lDir :: Vec
                 } deriving (Show, Eq)

instance Arbitrary Line where
        arbitrary = do point <- arbitrary
                       NormalizedV dir <- arbitrary
                       return $ Line point dir

-- | Ray defined by point and direction
data Ray = Ray { rPoint :: Point
               , rDir :: Vec
               } deriving (Show, Eq)

instance Arbitrary Ray where
        arbitrary = do point <- arbitrary
                       NormalizedV dir <- arbitrary
                       return $ Ray point dir

-- | Plane defined by point and normal
data Plane = Plane { planeNormal :: Vec
                   , planePoint :: Point
                   } deriving (Show, Eq)

instance Arbitrary Plane where
        arbitrary = do point <- arbitrary
                       NormalizedV normal <- arbitrary
                       return $ Plane point normal

-- | Face defined by normal and three vertices
data Face = Face { faceNormal :: Point
                 , faceVertices :: (Point,Point,Point)
                 } deriving (Show, Eq)
                 
faceFromVertices vs@(v0,v1,v2) = let u = v1 ^-^ v0
                                     v = v2 ^-^ v1
                                 in Face (normalized $ u `cross3` v) vs

instance Arbitrary Face where
        arbitrary = do NonZero vs <- arbitrary
                       return $ faceFromVertices vs

-- | Closed polygon defined by a series of connected points
type Polygon = [Point]

