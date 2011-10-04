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

-- | A contiguous path of line segments
type LineSegPath = [LineSeg]

-- | Line defined by point and direction
data Line = Line { lPoint :: Point
                 , lDir :: Vec
                 } deriving (Show, Eq)

-- | Ray defined by point and direction
data Ray = Ray { rPoint :: Point
               , rDir :: Vec
               } deriving (Show, Eq)

-- | Plane defined by point and normal
data Plane = Plane { planeNormal :: Vec
                   , planePoint :: Point
                   } deriving (Show, Eq)

-- | Face defined by normal and three vertices
data Face = Face { faceNormal :: Point
                 , faceVertices :: (Point,Point,Point)
                 } deriving (Show, Eq)

-- | Closed polygon defined by a series of connected points
type Polygon = [Point]

