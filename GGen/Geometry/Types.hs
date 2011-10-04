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

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Utils (normalize)

import Test.QuickCheck
import Numeric.LinearAlgebra.QuickCheck
import Control.Monad (liftM2)

-- | The maximum distance between identical points
pointTol = 1e-2 :: Double

-- | The maximum deviation from one in a dot product to consider vectors parallel
dirTol = 1e-3 :: Double


-- | Spatial vector
type Vec = Vector Double

-- | Are two vectors parallel to within dirTol
sameDir :: Vec -> Vec -> Bool
sameDir a b = abs (normalize a `dot` normalize b) - 1 < dirTol

-- | Spatial point
type Point = Vec

-- | Are two points the same to within pointTol?
samePoint :: Point -> Point -> Bool
samePoint a b = norm2 (a-b) < pointTol

-- | Line segment defined by two terminal points
data LineSeg = LineSeg { lsBegin :: Point
                       , lsEnd ::Point
                       } deriving (Show, Eq)

instance Arbitrary LineSeg where arbitrary = (liftM2 LineSeg) arbitrary arbitrary

-- | A contiguous path of line segments
type LineSegPath = [LineSeg]

-- | Line defined by point and direction
data Line = Line { lPoint :: Point
                 , lDir :: Vec
                 } deriving (Show, Eq)

instance Arbitrary Line where arbitrary = (liftM2 Line) arbitrary arbitrary

-- | Ray defined by point and direction
data Ray = Ray { rPoint :: Point
               , rDir :: Vec
               } deriving (Show, Eq)

instance Arbitrary Ray where arbitrary = (liftM2 Ray) arbitrary arbitrary

-- | Plane defined by point and normal
data Plane = Plane { planeNormal :: Vec
                   , planePoint :: Point
                   } deriving (Show, Eq)

instance Arbitrary Plane where arbitrary = (liftM2 Plane) arbitrary arbitrary

-- | Face defined by normal and three vertices
data Face = Face { faceNormal :: Point
                 , faceVertices :: (Point,Point,Point)
                 } deriving (Show, Eq)
                 
instance Arbitrary Face where arbitrary = (liftM2 Face) arbitrary arbitrary

-- | Closed polygon defined by a series of connected points
type Polygon = [Point]

