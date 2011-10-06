{-# LANGUAGE TemplateHaskell #-}

module GGen.Geometry.Intersect ( rayLineSegIntersect
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
import Data.Maybe (mapMaybe, isJust, fromJust)
import Control.Monad (when)
import qualified GGen.Pretty as P
import GGen.Pretty (($$), (<+>))
import Data.List (nubBy)

import Test.QuickCheck.All
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers (NonZero(..))
import Data.VectorSpace.QuickCheck
import Data.Cross

-- | Point of intersection between a ray and a line segment
rayLineSegIntersect :: Ray -> LineSeg -> Intersection Point
rayLineSegIntersect ray@(Ray {rBegin=a, rDir=b}) l@(LineSeg u v)
        | parallel d v  = if t >= 0 && t' >= 0 && t' <= 1 then IDegenerate
                                                          else INull
        | otherwise     = if t >= 0 && t' >= 0 && t' <= 1 then IIntersect (u + t' *^ v)
                                                          else INull
        where vv = magnitudeSq v
              bb = magnitudeSq b
              -- Length along line segment
              t' = (v ^/ vv) <.> (a-u) - (b <.> (v ^/ vv)) * ((a-u) <.> (b ^/ bb))
                   / (1 - (b <.> v)^2 / (vv*bb))
              t  = ((t' *^ v - (a-u)) <.> b) / bb -- Length along ray
              d = trace (show t'++"\t"++show t) $ lineSegDispl l

-- | Point of intersection between a face and a line
-- Using Moeller, Trumbore (1997)
faceLineIntersect :: Face -> Line -> Intersection Point
faceLineIntersect (Face {faceVertices=(v0,v1,v2)}) (Line {lPoint=p, lDir=d}) =
        do let u = v1 - v0
               v = v2 - v0
               pp = d `cross3` v
               det = u <.> pp
           when (abs det < 1e-5) INull
           let tt = p - v0
               uu = (tt <.> pp) / det
           when (uu < 0 || uu > 1) INull
           let qq = tt `cross3` u
               vv = (d <.> qq) / det
           when (vv < 0 || uu+vv > 1) INull
           let t = (v <.> qq) / det
           return $ p + t *^ d
        
-- | Check whether a point sits on a plane
pointOnPlane :: Plane -> Point -> Bool
pointOnPlane (Plane {planePoint=v, planeNormal=n}) p =
        abs ((p-v) <.> n) < 1e-8

-- | Find point of intersection of a plane and line
planeLineIntersect :: Plane -> Line -> Intersection Point
planeLineIntersect plane line@(Line {lPoint=a, lDir=m})
        | planeNormal plane `perpendicular` m && pointOnPlane plane a =
                IDegenerate
        | otherwise = 
                IIntersect $ a + m ^* planeLineIntersect' plane line

-- | Find value of parameter t of the line $\vec r = \vec A*t + \vec B$ for the
-- intersection with plane
planeLineIntersect' :: Plane -> Line -> Double
planeLineIntersect' (Plane {planeNormal=n, planePoint=v0}) (Line{lPoint=a, lDir=m}) =
        (n <.> (v0 - a)) / (n <.> m)

tr x = trace (show x) x
-- | Point of intersection between plane and line segment
planeLineSegIntersect :: Plane -> LineSeg -> Intersection Point
planeLineSegIntersect (Plane {planeNormal=n, planePoint=v}) (LineSeg a b)
        | perpendicular n (b-a)  = IDegenerate
        | t < 0                  = INull
        | t > 1                  = INull
        | otherwise              = IIntersect $ lerp a b t
        where t = (n <.> (v-a)) / (n <.> (b-a))

-- | Eliminate duplicate coincident points
nubPoints = nubBy coincident

-- | Line segment of intersection between plane and face
planeFaceIntersect :: Plane -> Face -> Intersection LineSeg
planeFaceIntersect plane face@(Face {faceVertices=(a,b,c)})
        | aOnPlane && bOnPlane && cOnPlane      = IDegenerate
        | aOnPlane && bOnPlane                  = IIntersect (LineSeg a b)
        | aOnPlane && cOnPlane                  = IIntersect (LineSeg a c)
        | bOnPlane && cOnPlane                  = IIntersect (LineSeg b c)
        | otherwise = 
                let lines = map (uncurry LineSeg) [(a,b), (b,c), (c,a)]
                    f l  | IIntersect p <- i  = Just p
                         | otherwise          = Nothing
                         where i = planeLineSegIntersect plane l
                    lineIntersects = nubPoints $ mapMaybe f lines
                    g = show $ P.text "planeFaceIntersect"
                            $$ P.nest 2 (  P.plane plane
                                        $$ P.face face <+> P.text "with normal" <+> P.vec (faceNormal face)
                                        $$ P.text "Intersects"
                                        $$ P.nest 2 (P.vcat $ map (P.text . show . planeLineSegIntersect plane) lines)
                                        )
                in case length lineIntersects of
                        0         -> INull
                        1         -> INull  -- TODO: Or perhaps IDegenerate?
                        2         -> IIntersect $ LineSeg (head lineIntersects) (last lineIntersects)
                        3         -> IDegenerate
                        otherwise -> error (show $ P.text "Error while finding plane-face intersection:"
                                                $$ P.text "Unexpected number of intersections"
                                                $$ P.nest 2 (P.vcat $ map P.point lineIntersects)
                                                $$ P.text "Face: " $$ P.nest 2 (P.face face)
                                                $$ P.text "Plane: " $$ P.nest 2 (P.plane plane))
        where aOnPlane = pointOnPlane plane a
              bOnPlane = pointOnPlane plane b
              cOnPlane = pointOnPlane plane c

-- | The in-plane normal vector for the intersection between a plane and face
planeFaceNormal :: Plane -> Face -> Vec
planeFaceNormal plane face =
        project (planeNormal plane) (faceNormal face)

-- QuickCheck properties

-- Properties for rayLineSegIntersect
-- | Check that rays and line segment intersections are found
prop_ray_line_seg_intersection_hit :: NonNull LineSeg -> Point -> Result
prop_ray_line_seg_intersection_hit (NonNull l@(LineSeg a b)) rayBegin
        | parallel rayDir (lineSegDispl l)   = rejected
        | otherwise = case rayLineSegIntersect (Ray {rBegin=rayBegin, rDir=rayDir}) l of
                                IIntersect i  -> if trace (show $ g i) $ coincident i intersect
                                                    then succeeded
                                                    else failed {reason="Incorrect intersection"}
                                otherwise     -> failed {reason="No intersection"}
        where intersect = lerp a b 0.5
              rayDir = intersect - rayBegin
              g i = P.text "intersect" <+> P.vec intersect
                 $$ P.text "i" <+> P.vec i

-- Properties for faceLineIntersect
-- | Check that face-line intersections are found
prop_face_line_intersection_hit :: Face -> NormalizedV Vec -> Normalized Double -> Result
prop_face_line_intersection_hit face@(Face {faceVertices=(v0,v1,v2)}) (NormalizedV dir) (Normalized a) = 
        let tol = 1e-8
            u = v1 - v0
            v = v2 - v0
            a' = (1-tol) * a -- Avoid numerical error near edges
            b' = (1-tol) - a'
            hitPoint = v0 + a' *^ u + b' *^ v
            origin = hitPoint - dir
        in case faceLineIntersect face (Line {lPoint=origin, lDir=dir}) of
                IIntersect intersect  -> liftBool $ magnitude (intersect - hitPoint) < 1e-5
                INull                 -> failed {reason="No intersection found"}

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
                IIntersect intersect  -> failed {reason="Found non-existent intersection"}
                INull                 -> succeeded

-- Properties for planeLineIntersect
-- | Check that line-plane intersection point falls on plane
prop_plane_line_intersection_on_plane :: Line -> Plane -> Bool
prop_plane_line_intersection_on_plane line plane@(Plane n v) =
        abs ((p - v) <.> n) < 1e-8
        where IIntersect p = planeLineIntersect plane line

-- | Check that line-plane intersection point falls on line
prop_plane_line_intersection_on_line :: Line -> Plane -> Bool
prop_plane_line_intersection_on_line line@(Line {lPoint=v, lDir=d}) plane = 
        magnitude (lambda *^ d + v - p) < 1e-8
        where IIntersect p = planeLineIntersect plane line
              lambda = (p - v) <.> d ^/ magnitudeSq d

-- | Check that line falling on plane is degenerate
prop_plane_line_intersection_degenerate :: Plane -> Point -> NonZero Point -> Bool
prop_plane_line_intersection_degenerate plane@(Plane {planeNormal=n, planePoint=p}) a (NonZero b) =
        case planeLineIntersect plane line of
             IDegenerate        -> True
             otherwise          -> False
        where b' = b - n ^* (n <.> b)
              line = Line {lPoint=p + a `cross3` n, lDir=normalized b'}

-- Properties for pointOnPlane
-- | Check that point on plane is recognized
prop_point_on_plane_hit :: Plane -> Vec -> Bool
prop_point_on_plane_hit plane v =
        pointOnPlane plane (planePoint plane + v')
        where n = planeNormal plane
              v' = v - n ^* (v <.> n)

-- | Check that point off plane is recognized
prop_point_on_plane_miss :: Plane -> NonZero Vec -> Result
prop_point_on_plane_miss plane@(Plane {planeNormal=n}) (NonZero v) 
        | abs (n <.> v) < 1e-8  = rejected
        | otherwise             = liftBool $ not $ pointOnPlane plane v

-- Properties for planeFaceIntersect
-- | Check that a face sitting in the plane is recognized as degenerate
prop_plane_face_intersect_face_in_plane :: Plane -> NonZero Vec -> NonZero Vec -> NonZero Vec -> Bool
prop_plane_face_intersect_face_in_plane plane (NonZero a) (NonZero b) (NonZero c) =
        case planeFaceIntersect plane face of
             IDegenerate        -> True
             otherwise          -> trace (show g) False
        where Plane {planePoint=p, planeNormal=n} = plane
              proj x = x - n ^* (n <.> x) -- Project onto plane
              vs = (p + proj a, p + proj b, p + proj c)
              face = faceFromVertices vs
              g = ""

-- | Check that a face intersection with a plane gives a line segment
prop_plane_face_intersect_face_intersects_plane :: Plane -> NonZero Point -> NonZero Point -> NonZero Point -> Result
prop_plane_face_intersect_face_intersects_plane plane (NonZero a) (NonZero b) (NonZero c)
        | parallel (faceNormal face) (planeNormal plane)  = rejected
        | otherwise  =
                case planeFaceIntersect plane face of
                     IIntersect _       -> succeeded
                     otherwise          -> trace (show $ planeFaceIntersect plane face) failed
        where d = planePoint plane - (a + b + c) ^/ 3   -- Displacement from center of points to planePoint
              face = faceFromVertices (a+d, b+d, c+d)

-- | Check face with one edge sitting on plane
prop_plane_face_intersect_edge_on_plane :: Plane -> NonZero Point -> NonZero Point -> NonZero Point -> Result
prop_plane_face_intersect_edge_on_plane plane (NonZero a) (NonZero b) (NonZero c) =
        case planeFaceIntersect plane face of
             IIntersect l   -> let d = lineSegDispl l
                                   d' = lineSegDispl (LineSeg a' b')
                               in if 1 - abs (d <.> d') > 1e-8 
                                     then failed {reason="Line of intersection has incorrect direction"}
                                     else if abs (magnitude d - magnitude d') > 1e-8
                                             then failed {reason="Line of intersection has incorrect magnitude"}
                                             else succeeded
             otherwise      -> failed {reason="No intersection found"}
        where Plane {planePoint=p, planeNormal=n} = plane
              proj x = x - n ^* (n <.> x) -- Project onto plane
              a' = p + proj a
              b' = p + proj b
              face = faceFromVertices (a', b', c)

-- Properties for planeLineSegIntersect
-- | Check that line sitting in plane is degenerate
prop_plane_line_seg_intersect_line_in_plane :: Plane -> NonZero Vec -> NonZero Vec -> Bool
prop_plane_line_seg_intersect_line_in_plane plane (NonZero a) (NonZero b) =
        case planeLineSegIntersect plane l of
             IDegenerate        -> True
             otherwise          -> False
        where Plane {planePoint=p, planeNormal=n} = plane
              proj x = x - n ^* (n <.> x) -- Project onto plane
              l = LineSeg (p + proj a) (p + proj b)

prop_plane_line_seg_intersect_hit :: Plane -> NonZero Vec -> Result
prop_plane_line_seg_intersect_hit plane (NonZero a)
        | perpendicular (planeNormal plane) a  = rejected
        | otherwise =
                case planeLineSegIntersect plane l of
                     IIntersect i  -> if magnitude (i-p) < 1e-8 then succeeded
                                                                else failed {reason="Incorrect intersection"}
                     otherwise     -> failed {reason="No intersection found"}
        where p = planePoint plane
              l = LineSeg (p+a) (p-a)

-- | Check that parallel line sitting out of plane doesn't intersect
--prop_plane_line_seg_intersect_parallel_off_plane :: Plane -> NonZero Vec -> NonNull LineSeg -> Bool
--prop_plane_line_seg_intersect_parallel_off_plane plane v l =
--        where Plane {planeNormal=n

runTests = $quickCheckAll

