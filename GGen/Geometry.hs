module GGen.Geometry where

import Data.Maybe (mapMaybe)
import Numeric.LinearAlgebra
import GGen.Types
import Data.STL.Binary

main = parse "z-tensioner_1off.stl" >>= print

-- | A group of polygons marked with whether their interiors should be filled
data BodySlice = BodySlice { bsPolys :: [(Polygon, Bool)] }

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

planeSlice :: Plane -> [Face] -> Polygon
planeSlice plane faces = undefined

