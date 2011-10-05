{-# LANGUAGE TemplateHaskell #-}

module GGen.Geometry.Intersect ( rayLineSegIntersect
                               , faceLineIntersect
                               , planeLineSegIntersect
                               , planeFaceIntersect
                               , planeFaceNormal
                               , GGen.Geometry.Intersect.runTests
                               ) where

import Data.VectorSpace
import GGen.Geometry.Types
import GGen.Geometry.LineSeg
import Data.Maybe (mapMaybe, isJust, fromJust)
import Control.Monad (when)

import Test.QuickCheck.All
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers (NonZero(..))
import Data.VectorSpace.QuickCheck
import Data.Cross

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

-- | Point of intersection between a face and a line
-- Using Moeller, Trumbore (1997)
faceLineIntersect :: Face -> Line -> Maybe Point
faceLineIntersect (Face {faceVertices=(v0,v1,v2)}) (Line {lPoint=p, lDir=d}) =
        do let u = v1 - v0
               v = v2 - v0
               pp = d `cross3` v
               det = u <.> pp
           when (abs det < 1e-5) Nothing
           let tt = p - v0
               uu = (tt <.> pp) / det
           when (uu < 0 || uu > 1) Nothing
           let qq = tt `cross3` u
               vv = (d <.> qq) / det
           when (vv < 0 || uu+vv > 1) Nothing
           let t = (v <.> qq) / det
           return $ p ^+^ t *^ d
        
-- | Find point of intersection of a plane and line
planeLineIntersect :: Plane -> Line -> Point
planeLineIntersect plane line@(Line {lPoint=a, lDir=m}) =
        a ^+^ m ^* planeLineIntersect' plane line

-- | Find value of parameter t of the line $\vec r = \vec A*t + \vec B$ for the
-- intersection with plane
planeLineIntersect' :: Plane -> Line -> Double
planeLineIntersect' (Plane {planeNormal=n, planePoint=v0}) (Line{lPoint=a, lDir=m}) =
        (n <.> (v0 ^-^ a)) / (n <.> m)

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

-- QuickCheck properties

-- | Check that face-line intersections are found
prop_face_line_intersection_hit :: Face -> NormalizedV Vec -> Normalized Double -> Result
prop_face_line_intersection_hit face@(Face {faceVertices=(v0,v1,v2)}) (NormalizedV dir) (Normalized a) = 
        let tol = 1e-8
            u = v1 - v0
            v = v2 - v0
            a' = (1-tol) * a -- Numerical error
            b' = (1-tol) - a'
            hitPoint = v0 + a' *^ u + b' *^ v
            origin = hitPoint - dir
        in case faceLineIntersect face (Line {lPoint=origin, lDir=dir}) of
                Just intersect -> liftBool $ magnitude (intersect - hitPoint) < 1e-5
                Nothing        -> failed {reason="No intersection found"}

-- | Check that only intersections are found
prop_face_line_intersection_miss :: Face -> NormalizedV Vec -> Normalized Double -> Result
prop_face_line_intersection_miss face@(Face {faceVertices=(v0,v1,v2)}) (NormalizedV dir) (Normalized a) =
        let tol = 1e-8
            u = v1 - v0
            v = v2 - v0
            a' = (1-tol) * a
            b' = (1-tol) * a'
            hitPoint = v0 - a' *^ u - b' *^ v
            origin = hitPoint - dir
        in case faceLineIntersect face (Line {lPoint=origin, lDir=dir}) of
                Just intersect -> failed {reason="Found non-existent intersection"}
                Nothing        -> succeeded

-- | Check that line-plane intersection point falls on plane
prop_plane_line_intersection_on_plane :: Line -> Plane -> Bool
prop_plane_line_intersection_on_plane line plane@(Plane n v) =
        abs ((p - v) <.> n) < 1e-8
        where p = planeLineIntersect plane line

-- | Check that line-plane intersection point falls on line
prop_plane_line_intersection_on_line :: Line -> Plane -> Bool
prop_plane_line_intersection_on_line line@(Line {lPoint=v, lDir=d}) plane = 
        magnitude (lambda *^ d + v - p) < 1e-8
        where p = planeLineIntersect plane line
              lambda = (p ^-^ v) <.> d ^/ magnitudeSq d

runTests = $quickCheckAll

