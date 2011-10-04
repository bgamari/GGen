
module GGen.Types ( Vec
                  , Point
                  , LineSeg(..)
                  , LineSegPath
                  , lineSegBegin
                  , lineSegEnd
                  , Line(..)
                  , Plane(..)
                  , Face(..)
                  , Polygon
                  , Ray(..)
                  ) where

import Numeric.LinearAlgebra

-- | Spatial vector
type Vec = Vector Double

-- | Spatial point
type Point = Vec

-- | Line segment defined by two terminal points
newtype LineSeg = LineSeg (Point, Point) deriving (Show, Eq)
type LineSegPath = [LineSeg]

lineSegBegin, lineSegEnd :: LineSeg -> Point
lineSegBegin (LineSeg (a,_)) = a
lineSegEnd (LineSeg (_,a)) = a

-- | Line defined by point and direction
newtype Line = Line (Point, Vec) deriving (Show, Eq)

-- | Ray defined by point and direction
newtype Ray = Ray (Point, Vec) deriving (Show, Eq)

-- | Plane defined by point and normal
data Plane = Plane { planeNormal :: Vec, planePoint :: Point }
                   deriving (Show, Eq)

-- | Face defined by normal and three vertices
data Face = Face { faceNormal :: Point
                 , faceVertices :: (Point,Point,Point) }
                 deriving (Show, Eq)

-- | Closed polygon defined by a series of connected points
type Polygon = [Point]

