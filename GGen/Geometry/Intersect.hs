module GGen.Geometry.Intersect ( rayLineSegIntersect
                               , pointOnFace
                               , faceLineIntersect
                               , planeLineSegIntersect
                               , planeFaceIntersect
                               , planeFaceNormal
                               ) where

import Data.VectorSpace
import GGen.Geometry.Types
import GGen.Geometry.LineSeg
import Data.Maybe (mapMaybe)

-- | Point of intersection between a ray and a line segment
rayLineSegIntersect :: Ray -> LineSeg -> Maybe Point
rayLineSegIntersect ray@(Ray u v) l@(LineSeg a b)
        | sameDir (lineSegDispl l) v  = 
                -- If they are parallel, we approximate the intersection as the
                -- center of the segment
                if t >= 0 && t' >= 0 && t' < 1 then Just $ a ^+^ (b ^-^ a) ^/ 2
                                               else Nothing
        | otherwise       = 
                if t >= 0 && t' >= 0 && t' < 1 then Just (u ^+^ t *^ v)
                                               else Nothing
        where t' = -(( (v <.> v) *^ (u ^-^ a) ^-^ ((u ^-^ a) <.> v) *^ v ) <.> (b ^-^ a)) / (v <.> (b ^-^ a))^2
              t  =  ((a ^-^ u ^+^ t' *^ (b ^-^ a)) <.> v) / (v <.> v)^2

-- | Test whether a point sits on a face
-- Based upon http://softsurfer.com/Archive/algorithm_0105/algorithm_0105.htm#Segment-Triangle
pointOnFace :: Face -> Point -> Bool
pointOnFace face p =
        let (v0,v1,v2) = faceVertices face
            u = v1 ^-^ v0
            v = v2 ^-^ v0
            w = p ^-^ v0
            uu = u <.> u
            vv = v <.> v
            uv = u <.> v
            wv = w <.> v
            wu = w <.> u
            s = (uv*wv - vv*wu) / (uv^2 - uu*vv)
            t = (uv*wu - uu*wv) / (uv^2 - uu*vv)
        in s >= 0 && t >= 0 && s+t <= 1

-- | Point of intersection between a face and a line
faceLineIntersect :: Face -> Line -> Maybe Point
faceLineIntersect face@(Face {faceNormal=n, faceVertices=(v,_,_)}) (Line a m)
        | pointOnFace face p    = Just p
        | otherwise             = Nothing
        where lambda = (n <.> (v ^-^ a)) / (n <.> m)
              p = a ^+^ lambda *^ m

-- | Point of intersection between plane and line segment
planeLineSegIntersect :: Plane -> LineSeg -> Maybe Point
planeLineSegIntersect (Plane {planeNormal=n, planePoint=v}) (LineSeg a b)
        | lambda < 0 = Nothing
        | lambda > 1 = Nothing
        | otherwise  = Just $ lerp a b lambda
        where lambda = (n <.> (v ^-^ a)) / (n <.> (b ^-^ a))

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
planeFaceNormal plane face = project (planeNormal plane) (faceNormal face)

