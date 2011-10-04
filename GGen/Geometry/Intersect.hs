module GGen.Geometry.Intersect ( rayLineSegIntersect
                               , pointOnFace
                               , faceLineIntersect
                               , planeLineSegIntersect
                               , planeFaceIntersect
                               , planeFaceNormal
                               ) where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Utils
import GGen.Geometry.Types
import GGen.Geometry.LineSeg
import Data.Maybe (mapMaybe)

-- | Point of intersection between a ray and a line segment
rayLineSegIntersect :: Ray -> LineSeg -> Maybe Point
rayLineSegIntersect ray@(Ray u v) l@(LineSeg a b)
        | sameDir (lineSegDispl l) v  = 
                -- If they are parallel, we approximate the intersection as the
                -- center of the segment
                if t >= 0 && t' >= 0 && t' < 1 then Just $ a + 0.5 `scale` (b-a)
                                               else Nothing
        | otherwise       = 
                if t >= 0 && t' >= 0 && t' < 1 then Just (u + t `scale` v)
                                               else Nothing
        where t' = -( (v `dot` v) `scale` (u-a) - ((u-a) `dot` v) `scale` v ) `dot` (b-a) / (v `dot` (b-a))^2
              t  = (a - u + t' `scale` (b-a)) `dot` v / (v `dot` v)^2

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
faceLineIntersect face@(Face {faceNormal=n, faceVertices=(v,_,_)}) (Line a m)
        | pointOnFace face p    = Just p
        | otherwise             = Nothing
        where lambda = (n `dot` (v-a)) / (n `dot` m)
              p = a + lambda `scale` m

-- | Point of intersection between plane and line segment
planeLineSegIntersect :: Plane -> LineSeg -> Maybe Point
planeLineSegIntersect (Plane {planeNormal=n, planePoint=v}) (LineSeg a b)
        | lambda < 0 = Nothing
        | lambda > 1 = Nothing
        | otherwise  = Just $ a + lambda `scale` (b-a)
        where lambda = (n `dot` (v-a)) / (n `dot` (b-a))

-- | Line segment of intersection between plane and face
planeFaceIntersect :: Plane -> Face -> Maybe LineSeg
planeFaceIntersect plane (Face {faceVertices=(a,b,c)}) =
        let lines = map (uncurry LineSeg) [(a,b), (b,c), (c,a)]
            lineIntersects = mapMaybe (planeLineSegIntersect plane) lines
        in case length lineIntersects of
                0         -> Nothing
                1         -> error "Only one intersection point"
                2         -> Just $ LineSeg (head lineIntersects) (last lineIntersects)
                otherwise -> error ("Unexpected number of intersections: "++show lineIntersects)

-- | The in-plane normal vector the intersection between a plane and face
planeFaceNormal :: Plane -> Face -> Vec
planeFaceNormal plane face =
           let fn = faceNormal face
               pn = planeNormal plane
           in fn - (fn `dot` pn) `scale` pn

