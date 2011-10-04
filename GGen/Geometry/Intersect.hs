{-# LANGUAGE TemplateHaskell #-}

module GGen.Geometry.Intersect ( rayLineSegIntersect
                               , pointOnFace
                               , faceLineIntersect
                               , planeLineSegIntersect
                               , planeFaceIntersect
                               , planeFaceNormal
                               , GGen.Geometry.Intersect.runTests
                               ) where

import Debug.Trace

import Data.VectorSpace
import GGen.Geometry.Types
import GGen.Geometry.LineSeg
import Data.Maybe (mapMaybe, isJust)

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

-- | Test whether a point sits on a face
-- Based upon http://www.cs.cornell.edu/courses/cs465/2003fa/homeworks/raytri.pdf
pointOnFace :: Face -> Point -> Bool
pointOnFace (Face {faceVertices=(v0,v1,v2), faceNormal=n}) p = 
           (v1 ^-^ v0) `cross3` (p ^-^ v0) <.> n >= 0
        && (v2 ^-^ v1) `cross3` (p ^-^ v1) <.> n >= 0
        && (v0 ^-^ v2) `cross3` (p ^-^ v2) <.> n >= 0

-- | Point of intersection between a face and a line
faceLineIntersect :: Face -> Line -> Maybe Point
faceLineIntersect face@(Face {faceNormal=n, faceVertices=(v0,_,_)}) line
        | pointOnFace face p  = Just p
        | otherwise           = Nothing
        where p = planeLineIntersect (Plane {planeNormal=n, planePoint=v0}) line
        
-- Find point of intersection of a plane and line
planeLineIntersect :: Plane -> Line -> Point
planeLineIntersect (Plane {planeNormal=n, planePoint=v0}) (Line {lPoint=a, lDir=m}) =
              let lambda = (n <.> (v0 ^-^ a)) / (n <.> m)
              in a ^+^ lambda *^ m

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

-- | Check that points on face are recognized
prop_point_on_face :: Face -> Normalized Double -> Normalized Double -> Result
prop_point_on_face face (Normalized x) (Normalized y)
        | x + y > 1     = rejected
        | otherwise     = if pointOnFace face p then succeeded
                                                else failed
                          where (v0,v1,v2) = faceVertices face
                                u = v1 ^-^ v0
                                v = v2 ^-^ v0
                                p = v0 ^+^ x *^ u ^+^ y *^ v

-- | Check that points off face are handled properly
prop_point_off_face :: Face -> NonZero Vec -> Result
prop_point_off_face face (NonZero v)
        | abs (v <.> n) < 1e-10     = rejected
        | otherwise                 = liftBool $ not $ pointOnFace face (p ^+^ v)
        where n = faceNormal face
              (p,_,_) = faceVertices face

-- | Check that line-plane intersection point falls on plane
prop_plane_line_intersection_on_plane :: Line -> Plane -> Bool
prop_plane_line_intersection_on_plane line plane@(Plane n v) =
        abs ((p ^-^ v) <.> n) < 1e-10
        where p = planeLineIntersect plane line

-- | Check that line-plane intersection point falls on line
prop_plane_line_intersection_on_line :: Line -> Plane -> Bool
prop_plane_line_intersection_on_line line@(Line {lPoint=v, lDir=d}) plane = 
        magnitude (lambda *^ d ^+^ v ^-^ p) < 1e-10
        where p = planeLineIntersect plane line
              lambda = (p ^-^ v) <.> d ^/ magnitudeSq d

runTests = $quickCheckAll

