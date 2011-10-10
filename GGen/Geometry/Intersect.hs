{-# LANGUAGE TemplateHaskell #-}

module GGen.Geometry.Intersect ( rayLineSeg2Intersect
                               , lineLineSeg2Intersect
                               , lineSegLineSeg2Intersect
                               , lineLine2Intersect
                               , faceLineIntersect
                               , planeLineSegIntersect
                               , planeFaceIntersect
                               , GGen.Geometry.Intersect.runTests
                               ) where

import Data.VectorSpace
import GGen.Geometry.Types
import GGen.Geometry.LineSeg
import Data.Maybe (mapMaybe, isJust, fromJust)
import Control.Monad (when)
import qualified GGen.Pretty as P
import GGen.Pretty (($$), (<+>))

import Test.QuickCheck.All
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers (NonZero(..))
import Data.VectorSpace.QuickCheck
import Data.Cross

-- | Point of intersection between a ray and a line segment in two dimensions
rayLineSeg2Intersect :: Ray Point2 -> LineSeg Point2 -> Intersection Point2
rayLineSeg2Intersect (Ray {rBegin=u, rDir=v}) l@(LineSeg a b)
        | t >= 0 && t' >= 0 && t' <= 1  = if parallel v m then IDegenerate
                                                          else IIntersect $ lerp a b t'
        | otherwise                     = INull
        where m = lsDispl l
              mm = magnitudeSq m
              vv = magnitudeSq v
              -- Length along line segment
              t' = ((m ^/ mm) <.> (u-a) - ((u-a) <.> (v ^/ vv)) * (v <.> (m ^/ mm)))
                   / (1 - (m <.> v)^2 / (mm*vv))
              t  = ((t' *^ m + (a-u)) <.> v) / vv -- Length along ray

-- | Point of intersection between a line and a line segment in two dimensions
lineLineSeg2Intersect :: Line Point2 -> LineSeg Point2 -> Intersection Point2
lineLineSeg2Intersect (Line {lPoint=u, lDir=v}) (LineSeg a b)
        | t' >= 0 && t' <= 1    = if parallel v m then IDegenerate
                                                  else IIntersect $ lerp a b t'
        | otherwise             = INull
        where m = b - a
              mm = magnitudeSq m
              vv = magnitudeSq v
              -- Length along line segment
              t' = ((m ^/ mm) <.> (u-a) - ((u-a) <.> (v ^/ vv)) * (v <.> (m ^/ mm)))
                   / (1 - (m <.> v)^2 / (mm*vv))
              t  = ((t' *^ m + (a-u)) <.> v) / vv -- Length along line

-- | Point of intersection between two line segments in two dimensions
lineSegLineSeg2Intersect :: LineSeg Point2 -> LineSeg Point2 -> Intersection Point2
lineSegLineSeg2Intersect u@(LineSeg ua ub) v@(LineSeg va vb)
        | a             = if parallel m n then IDegenerate
                                          else IIntersect $ lerp va vb tv
        | otherwise     = INull
        where m = lsDispl u 
              mm = magnitudeSq m
              n = lsDispl v
              nn = magnitudeSq n
              -- Length along line segment u
              tu = ((m ^/ mm) <.> (va-ua) + ((ua-va) <.> (n ^/ nn)) * (n <.> (m ^/ mm)))
                   / (1 - (m <.> n)^2 / (mm*nn))
              tv = ((tu *^ m + (ua-va)) <.> n) / nn -- Length along line segment v
              a = tu >= 0 && tu <= 1 && tv >= 0 && tv <= 1

lineLine2Intersect :: Line Point2 -> Line Point2 -> Intersection Point2
lineLine2Intersect u v
        | ua == va && m `parallel` n    = IDegenerate
        | otherwise                     = IIntersect $ ua + tu *^ m
        where Line {lPoint=ua, lDir=m} = u
              Line {lPoint=va, lDir=n} = v
              mm = magnitudeSq m
              nn = magnitudeSq n
              -- Length along line u
              tu = ((m ^/ mm) <.> (va-ua) + ((ua-va) <.> (n ^/ nn)) * (n <.> (m ^/ mm)))
                   / (1 - (m <.> n)^2 / (mm*nn))
              tv = ((tu *^ m + (ua-va)) <.> n) / nn -- Length along line v

-- | Point of intersection between a face and a line
-- Using Moeller, Trumbore (1997)
faceLineIntersect :: Face -> Line Point -> Intersection Point
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
pointOnPlane :: Plane Point -> Point -> Bool
pointOnPlane (Plane {planePoint=v, planeNormal=n}) p =
        abs ((p-v) <.> n) < 1e-8

-- | Find point of intersection of a plane and line
planeLineIntersect :: Plane Point -> Line Point -> Intersection Point
planeLineIntersect plane line@(Line {lPoint=a, lDir=m})
        | planeNormal plane `perpendicular` m && pointOnPlane plane a =
                IDegenerate
        | otherwise = 
                IIntersect $ a + m ^* planeLineIntersect' plane line

-- | Find value of parameter t of the line $\vec r = \vec A*t + \vec B$ for the
-- intersection with plane
planeLineIntersect' :: Plane Point -> Line Point -> Double
planeLineIntersect' (Plane {planeNormal=n, planePoint=v0}) (Line{lPoint=a, lDir=m}) =
        (n <.> (v0 - a)) / (n <.> m)

-- | Point of intersection between plane and line segment
planeLineSegIntersect :: Plane Point -> LineSeg Point -> Intersection Point
planeLineSegIntersect plane (LineSeg a b)
        | perpendicular n (b-a)
          && pointOnPlane plane a  = IDegenerate
        | t < 0                    = INull
        | t > 1                    = INull
        | otherwise                = IIntersect $ lerp a b t
        where Plane {planeNormal=n, planePoint=v} = plane
              t = (n <.> (v-a)) / (n <.> (b-a))

-- | Line segment of intersection between plane and face
planeFaceIntersect :: Plane Point -> Face -> Intersection (LineSeg Point)
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
                in case length lineIntersects of
                        0         -> INull
                        1         -> INull  -- TODO: Or perhaps IDegenerate?
                        2         -> IIntersect $ LineSeg (head lineIntersects) (last lineIntersects)
                        3         -> IDegenerate
                        otherwise -> error (show $ P.text "Error while finding plane-face intersection:"
                                                $$ P.text "Unexpected number of intersections"
                                                $$ P.nest 2 (P.vcat $ map P.vec lineIntersects)
                                                $$ P.text "Face: " $$ P.nest 2 (P.face face)
                                                $$ P.text "Plane: " $$ P.nest 2 (P.plane plane))
        where aOnPlane = pointOnPlane plane a
              bOnPlane = pointOnPlane plane b
              cOnPlane = pointOnPlane plane c

-- QuickCheck properties

-- Properties for rayLineSeg2Intersect
-- | Check that ray-line segment intersections are found
prop_ray_line_seg2_intersection_hit :: NonNull (LineSeg Point2) -> Point2 -> Normalized Double -> Result
prop_ray_line_seg2_intersection_hit (NonNull l@(LineSeg a b)) rayBegin (Normalized t)
        | parallel rayDir (lsDispl l)   = rejected
        | otherwise = case rayLineSeg2Intersect (Ray {rBegin=rayBegin, rDir=rayDir}) l of
                                IIntersect i  -> if coincident i intersect
                                                    then succeeded
                                                    else failed {reason="Incorrect intersection"}
                                otherwise     -> failed {reason="No intersection"}
        where intersect = lerp a b t
              rayDir = normalized $ intersect - rayBegin

-- Properties for lineSegLineSeg2Intersect
-- | Check that line segment-line segment intersections are found
prop_line_seg_line_seg2_intersection_hit :: NonNull (LineSeg Point2) -> Point2 -> Normalized Double -> Result
prop_line_seg_line_seg2_intersection_hit (NonNull l@(LineSeg a b)) v (Normalized t)
        | parallel (lsDispl l) (lsDispl l')   = rejected
        | otherwise = case lineSegLineSeg2Intersect l l' of
                                IIntersect i  -> if coincident i intersect
                                                    then succeeded
                                                    else failed {reason="Incorrect intersection"}
                                otherwise     -> failed {reason="No intersection"}
        where intersect = lerp a b t
              l' = LineSeg {lsA=v, lsB=2 *^ intersect - v}

-- Properties for lineLine2Intersect
-- | Check that intersect falls on both lines
prop_line_line2_intersection_on_both :: Line Point2 -> Line Point2 -> Result
prop_line_line2_intersection_on_both u@(Line {lPoint=up, lDir=ud}) v@(Line {lPoint=vp, lDir=vd})
        | vd `parallel` ud     = rejected
        | otherwise = case lineLine2Intersect u v of
                IIntersect i  -> let t  = ((i - up) <.> ud) / magnitudeSq ud
                                     t' = ((i - vp) <.> vd) / magnitudeSq vd
                                 in liftBool $ magnitude (up + ud^*t - i) < 1e-8
                                            && magnitude (vp + vd^*t' - i) < 1e-8

-- Properties for faceLineIntersect
-- | Check that face-line intersections are found
prop_face_line_intersection_hit :: Face -> NormalizedV NVec -> Normalized Double -> Result
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
prop_face_line_intersection_miss :: Face -> NormalizedV NVec -> Normalized Double -> Result
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
prop_plane_line_intersection_on_plane :: Line Point -> Plane Point -> Bool
prop_plane_line_intersection_on_plane line plane@(Plane n v) =
        abs ((p - v) <.> n) < 1e-8
        where IIntersect p = planeLineIntersect plane line

-- | Check that line-plane intersection point falls on line
prop_plane_line_intersection_on_line :: Line Point -> Plane Point -> Bool
prop_plane_line_intersection_on_line line@(Line {lPoint=v, lDir=d}) plane = 
        magnitude (lambda *^ d + v - p) < 1e-8
        where IIntersect p = planeLineIntersect plane line
              lambda = (p - v) <.> d ^/ magnitudeSq d

-- | Check that line falling on plane is degenerate
prop_plane_line_intersection_degenerate :: Plane Point -> Point -> NonZero Point -> Bool
prop_plane_line_intersection_degenerate plane@(Plane {planeNormal=n, planePoint=p}) a (NonZero b) =
        case planeLineIntersect plane line of
             IDegenerate        -> True
             otherwise          -> False
        where b' = b - n ^* (n <.> b)
              line = Line {lPoint=p + a `cross3` n, lDir=normalized b'}

-- Properties for pointOnPlane
-- | Check that point on plane is recognized
prop_point_on_plane_hit :: Plane Point -> Vec -> Bool
prop_point_on_plane_hit plane v =
        pointOnPlane plane (planePoint plane + v')
        where n = planeNormal plane
              v' = v - n ^* (v <.> n)

-- | Check that point off plane is recognized
prop_point_on_plane_miss :: Plane Point -> NonZero Vec -> Result
prop_point_on_plane_miss plane@(Plane {planeNormal=n}) (NonZero v) 
        | abs (n <.> v) < 1e-8  = rejected
        | otherwise             = liftBool $ not $ pointOnPlane plane v

-- Properties for planeFaceIntersect
-- | Check that a face sitting in the plane is recognized as degenerate
prop_plane_face_intersect_face_in_plane :: Plane Point -> NonZero Vec -> NonZero Vec -> NonZero Vec -> Bool
prop_plane_face_intersect_face_in_plane plane (NonZero a) (NonZero b) (NonZero c) =
        case planeFaceIntersect plane face of
             IDegenerate        -> True
             otherwise          -> False
        where Plane {planePoint=p, planeNormal=n} = plane
              proj x = x - n ^* (n <.> x) -- Project onto plane
              vs = (p + proj a, p + proj b, p + proj c)
              face = faceFromVertices vs

-- | Check that a face intersection with a plane gives a line segment
prop_plane_face_intersect_face_intersects_plane :: Plane Point -> NonZero Point -> NonZero Point -> NonZero Point -> Result
prop_plane_face_intersect_face_intersects_plane plane (NonZero a) (NonZero b) (NonZero c)
        | parallel (faceNormal face) (planeNormal plane)  = rejected
        | otherwise  =
                case planeFaceIntersect plane face of
                     IIntersect _       -> succeeded
                     otherwise          -> failed
        where d = planePoint plane - (a + b + c) ^/ 3   -- Displacement from center of points to planePoint
              face = faceFromVertices (a+d, b+d, c+d)

-- | Check face with one edge sitting on plane
prop_plane_face_intersect_edge_on_plane :: Plane Point -> NonZero Point -> NonZero Point -> NonZero Point -> Result
prop_plane_face_intersect_edge_on_plane plane (NonZero a) (NonZero b) (NonZero c) =
        case planeFaceIntersect plane face of
             IIntersect l   -> let d = lsDispl l
                                   d' = lsDispl (LineSeg a' b')
                               in if 1 - abs (d <.> d') > 1e-8 
                                     then failed {reason="Line of intersection has incorrect direction"}
                                     else if abs (magnitude d - magnitude d') > 1e-8
                                             then failed {reason="Line of intersection has incorrect magnitude"}
                                             else succeeded
             otherwise      -> failed {reason="No intersection found"}
        where Plane {planePoint=p, planeNormal=n} = plane
              a' = p + projInPlane plane a
              b' = p + projInPlane plane b
              face = faceFromVertices (a', b', c)

-- Properties for planeLineSegIntersect
-- | Check that line sitting in plane is degenerate
prop_plane_line_seg_intersect_line_in_plane :: Plane Point -> NonZero Vec -> NonZero Vec -> Bool
prop_plane_line_seg_intersect_line_in_plane plane (NonZero a) (NonZero b) =
        case planeLineSegIntersect plane l of
             IDegenerate        -> True
             otherwise          -> False
        where Plane {planePoint=p, planeNormal=n} = plane
              proj = projInPlane plane
              l = LineSeg (p + proj a) (p + proj b)

-- | Check that non-degenerate plane-line segment intersections work
prop_plane_line_seg_intersect_hit :: Plane Point -> NonZero Vec -> Result
prop_plane_line_seg_intersect_hit plane (NonZero a)
        | perpendicular (planeNormal plane) a  = rejected
        | otherwise =
                case planeLineSegIntersect plane l of
                     IIntersect i  -> if magnitude (i-p) < 1e-8 then succeeded
                                                                else failed {reason="Incorrect intersection"}
                     otherwise     -> failed {reason="No intersection found"}
        where p = planePoint plane
              l = LineSeg (p+a) (p-a)

-- | Check that line parallel to plane sitting out of plane doesn't intersect
prop_plane_line_seg_intersect_parallel_off_plane :: Plane Point -> NonZero Double -> Vec -> NonZero Vec -> Result
prop_plane_line_seg_intersect_parallel_off_plane plane (NonZero s) a (NonZero b) =
        case planeLineSegIntersect plane l of
             INull        -> succeeded
             otherwise    -> failed
        where Plane {planeNormal=n, planePoint=p} = plane
              a' = p + n ^* s + projInPlane plane a
              b' = a' + projInPlane plane b
              l = LineSeg a' b'

runTests = $quickCheckAll

