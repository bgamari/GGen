module GGen.Geometry where

import Data.Maybe (mapMaybe)
import Data.List (sortBy, delete)
import Control.Monad (guard)
import Numeric.LinearAlgebra
import GGen.Types

-- | Test whether a point sits on a face
-- Based upon http://softsurfer.com/Archive/algorithm_0105/algorithm_0105.htm#Segment-Triangle
pointOnFace :: Face -> Point -> Bool
pointOnFace face p =
        let (v0,v1,v2) = faceVertices face
            u = v1 - v0
            v = v2 - v0
            w = p - v0
            uu = u `dot` u
            vv = v `dot` v
            uv = u `dot` v
            wv = w `dot` v
            wu = w `dot` u
            s = (uv*wv - vv*wu) / (uv^2 - uu*vv)
            t = (uv*wu - uu*wv) / (uv^2 - uu*vv)
        in s >= 0 && t >= 0 && s+t <= 1

-- | Point of intersection between a face and a line
faceLineIntersect :: Face -> Line -> Maybe Point
faceLineIntersect face@(Face {faceNormal=n, faceVertices=(v,_,_)}) (Line (a,m))
        | pointOnFace face p    = Just p
        | otherwise             = Nothing
        where lambda = (n `dot` (v-a)) / (n `dot` m)
              p = a + lambda `scale` m

-- | Point of intersection between plane and line segment
planeLineSegIntersect :: Plane -> LineSeg -> Maybe Point
planeLineSegIntersect (Plane {normal=n, point=v}) (LineSeg (a,b))
        | lambda < 0 = Nothing
        | lambda > 1 = Nothing
        | otherwise  = Just $ a + lambda `scale` (b-a)
        where lambda = (n `dot` (v-a)) / (n `dot` (b-a))

-- | Line segment of intersection between plane and face
planeFaceIntersect :: Plane -> Face -> Maybe LineSeg
planeFaceIntersect plane (Face {faceVertices=(a,b,c)}) =
        let lines = map LineSeg [(a,b), (b,c), (c,a)]
            lineIntersects = mapMaybe (planeLineSegIntersect plane) lines
        in case length lineIntersects of
                0         -> Nothing
                1         -> error "Only one intersection point"
                2         -> Just $ LineSeg (head lineIntersects, last lineIntersects)
                otherwise -> error "Unexpected number of intersections"

-- | A group of polygons marked with whether their interiors should be filled
data BodySlice = BodySlice { bsPolys :: [(Polygon, Bool)] }
                           deriving (Show)

-- | The maximum distance between identical points
pointTol = 1e-4

-- | Try to match up a set of line segments into a closed polygon
lineSegsToPolygon :: [LineSeg] -> Maybe Polygon
lineSegsToPolygon [] = Just []
lineSegsToPolygon (LineSeg (a,_):segs) = lineSegsToPolygon' [a] segs

lineSegsToPolygon' :: Polygon -> [LineSeg] -> Maybe Polygon
lineSegsToPolygon' poly []
        | dist < pointTol  = Just poly
        | otherwise        = Nothing
        where start = last poly
              end = head poly
              dist = norm2 (end-start)

lineSegsToPolygon' poly@(p:_) segs =
        do let targets = segs ++ map invertLineSeg segs -- List of possible next line segments
               distanceAndTarget seg@(LineSeg (x,_)) = (norm2 (p-x), seg)
               distTargets = sortBy (\(da,_) (db,_) -> compare da db)
                           $ filter ((<pointTol).fst)
                           $ map distanceAndTarget targets
           guard (not $ null distTargets)
           let target@(LineSeg (a,b)) = snd $ head distTargets
           lineSegsToPolygon' (b:a:poly) (delete target segs)

-- | Reverse the order of line segment termini
invertLineSeg :: LineSeg -> LineSeg
invertLineSeg (LineSeg (a,b)) = LineSeg (b,a)

-- | Try to find the boundaries sitting in a plane
planeSlice :: Plane -> [Face] -> Maybe Polygon
planeSlice plane faces = let boundaries = mapMaybe (planeFaceIntersect plane) faces
                         in lineSegsToPolygon boundaries

