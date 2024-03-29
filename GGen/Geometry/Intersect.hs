{-# LANGUAGE TemplateHaskell, FlexibleContexts, PatternGuards #-}

module GGen.Geometry.Intersect ( rayLineSeg2Intersect
                               , lineLineSeg2Intersect
                               , lineSegLineSeg2Intersect
                               , lineLine2Intersect
                               , faceLineIntersect
                               , planeLineSegIntersect
                               , planeFaceIntersect
                               , GGen.Geometry.Intersect.runTests
                               ) where

import  Debug.Trace
import Data.VectorSpace
import Data.AffineSpace
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

-- | Test whether a point falls on a line
pointOnLine :: (InnerSpace v, RealFloat (Scalar v)) => Point v -> Line v -> Bool
pointOnLine p (Line {lPoint=p'}) | p `coincident` p'  = True
pointOnLine (P p) (Line {lPoint=P a, lDir=m}) | magnitude p == 0  =
        a `parallel` m
pointOnLine (P r) (Line {lPoint=P a, lDir=m}) =
        let t = (magnitudeSq r - r <.> a) / (r <.> m)
        in P r `coincident` (P a .+^ t *^ m)

-- | Point of intersection between a ray and a line segment in two dimensions
rayLineSeg2Intersect :: Ray R2 -> LineSeg R2 -> Intersection P2
rayLineSeg2Intersect u v@(LineSeg va vb)
        | IIntersect (ut, vt) <- i      = if ut >~ 0 && vt >~ 0 && vt <= 1
                                            then IIntersect $ va .+^ lsDispl v ^* vt
                                            else INull
        | IDegenerate <- i              = IDegenerate
        | INull <- i                    = INull
        where i = lineLine2Intersect' (Line {lPoint=rBegin u, lDir=rDir u}) (Line {lPoint=va, lDir=lsDispl v})

-- | Point of intersection between a line and a line segment in two dimensions
lineLineSeg2Intersect :: Line R2 -> LineSeg R2 -> Intersection P2
lineLineSeg2Intersect l v@(LineSeg va vb)
        | IIntersect (ut, vt) <- i      = if vt >~ 0 && vt <~ 1
                                            then IIntersect $ va .+^ lsDispl v ^* vt
                                            else INull
        | IDegenerate <- i              = IDegenerate
        | INull <- i                    = INull
        where i = lineLine2Intersect' l (Line {lPoint=va, lDir=lsDispl v})

-- | Point of intersection between two line segments in two dimensions
lineSegLineSeg2Intersect :: LineSeg R2 -> LineSeg R2 -> Intersection P2
lineSegLineSeg2Intersect u@(LineSeg ua ub) v@(LineSeg va vb)
        | IIntersect (ut, vt) <- i      = if ut >~ 0 && ut <~ 1 && vt >~ 0 && vt <~ 1
                                            then IIntersect $ ua .+^ lsDispl u ^* ut
                                            else INull
        | IDegenerate <- i              = let la = lsDispl u <.> (ua .-. va)
                                              lb = lsDispl u <.> (ua .-. vb)
                                          in case () of
                                                _ | la*lb < 0  -> IDegenerate
                                                _ | otherwise  -> INull
        | INull <- i                    = INull
        where i = lineLine2Intersect' (Line {lPoint=ua, lDir=lsDispl u}) (Line {lPoint=va, lDir=lsDispl v})

-- | Point of intersection between two lines in two dimensions
lineLine2Intersect :: Line R2 -> Line R2 -> Intersection P2
lineLine2Intersect u@(Line {lPoint=ua, lDir=m}) v 
        | IIntersect (tu, tv) <- i      = IIntersect $ ua .+^ tu *^ m 
        | IDegenerate <- i              = IDegenerate
        | INull <- i                    = INull
        where i = lineLine2Intersect' u v

-- | Parameters (tu, tv) of intersection between two lines in two dimensions
lineLine2Intersect' :: Line R2 -> Line R2 -> Intersection (Double,Double)
lineLine2Intersect' u v
        | m `parallel` n && ua `pointOnLine` v  = IDegenerate
        | otherwise                             = IIntersect (tu, tv)
        where Line {lPoint=ua, lDir=m} = u
              Line {lPoint=va, lDir=n} = v
              mm = magnitudeSq m
              nn = magnitudeSq n
              -- Length along line u
              tu = ((m ^/ mm) <.> (va.-.ua) + ((ua.-.va) <.> (n ^/ nn)) * (n <.> (m ^/ mm)))
                   / (1 - (m <.> n)^2 / (mm*nn))
              tv = ((tu *^ m + (ua.-.va)) <.> n) / nn -- Length along line v

-- | Point of intersection between a face and a line
-- Using Moeller, Trumbore (1997)
faceLineIntersect :: Face -> Line R3 -> Intersection P3
faceLineIntersect (Face {faceVertices=(P v0,P v1,P v2)}) (Line {lPoint=P p, lDir=d}) =
        do let u = v1 - v0
               v = v2 - v0
               pp = d `cross3` v
               det = u <.> pp
           when (abs det <~ 0) INull
           let tt = p - v0
               uu = (tt <.> pp) / det
           when (uu <~ 0 || uu >~ 1) INull
           let qq = tt `cross3` u
               vv = (d <.> qq) / det
           when (vv <~ 0 || uu+vv >~ 1) INull
           let t = (v <.> qq) / det
           return $ P $ p + t *^ d

-- | Check whether a point sits on a plane
pointOnPlane :: Plane R3 -> P3 -> Bool
pointOnPlane (Plane {planePoint=v, planeNormal=n}) p =
        abs ((p.-.v) <.> n) =~ 0

-- | Find point of intersection of a plane and line
planeLineIntersect :: Plane R3 -> Line R3 -> Intersection P3
planeLineIntersect plane line@(Line {lPoint=a, lDir=m})
        | planeNormal plane `perpendicular` m && pointOnPlane plane a =
                IDegenerate
        | otherwise = 
                IIntersect $ a .+^ m ^* planeLineIntersect' plane line

-- | Find value of parameter t of the line $\vec r = \vec A*t + \vec B$ for the
-- intersection with plane
planeLineIntersect' :: Plane R3 -> Line R3 -> Double
planeLineIntersect' (Plane {planeNormal=n, planePoint=v0}) (Line{lPoint=a, lDir=m}) =
        (n <.> (v0 .-. a)) / (n <.> m)

-- | Point of intersection between plane and line segment
planeLineSegIntersect :: Plane R3 -> LineSeg R3 -> Intersection P3
planeLineSegIntersect plane (LineSeg a b)
        | perpendicular n (b.-.a)
          && pointOnPlane plane a  = IDegenerate
        | t < 0                    = INull
        | t > 1                    = INull
        | otherwise                = IIntersect $ alerp a b t
        where Plane {planeNormal=n, planePoint=v} = plane
              t = (n <.> (v.-.a)) / (n <.> (b.-.a))

-- | Line segment of intersection between plane and face
planeFaceIntersect :: Plane R3 -> Face -> Intersection (LineSeg R3)
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
                                                $$ P.nest 2 (P.vcat $ map P.point lineIntersects)
                                                $$ P.text "Face: " $$ P.nest 2 (P.face face)
                                                $$ P.text "Plane: " $$ P.nest 2 (P.plane plane))
        where aOnPlane = pointOnPlane plane a
              bOnPlane = pointOnPlane plane b
              cOnPlane = pointOnPlane plane c

-- QuickCheck properties

-- Properties for rayLineSeg2Intersect
-- | Check that ray-line segment intersections are found
prop_ray_line_seg2_intersection_hit :: NonNull (LineSeg R2) -> P2 -> Normalized Double -> Result
prop_ray_line_seg2_intersection_hit (NonNull l@(LineSeg a b)) rayBegin (Normalized t)
        | rayDir `parallel` (lsDispl l)   = rejected
        | otherwise = case rayLineSeg2Intersect (Ray {rBegin=rayBegin, rDir=rayDir}) l of
                                IIntersect i  -> if coincident i intersect
                                                    then succeeded
                                                    else failed {reason="Incorrect intersection"}
                                otherwise     -> failed {reason="No intersection"}
        where intersect = alerp a b t
              rayDir = normalized $ intersect .-. rayBegin

-- Properties for lineSegLineSeg2Intersect
-- | Check that line segment-line segment intersections are found
prop_line_seg_line_seg2_intersection_hit :: NonNull (LineSeg R2) -> P2 -> Normalized Double -> Result
prop_line_seg_line_seg2_intersection_hit (NonNull l@(LineSeg a b)) v (Normalized t)
        | parallel (lsDispl l) (lsDispl l')   = rejected
        | otherwise = case lineSegLineSeg2Intersect l l' of
                                IIntersect i  -> if coincident i intersect
                                                    then succeeded
                                                    else failed {reason="Incorrect intersection"}
                                otherwise     -> failed {reason="No intersection"}
        where intersect = alerp a b t
              l' = LineSeg {lsA=v, lsB=v .+^ 2*(intersect .-. v)}

-- Properties for lineLine2Intersect
-- | Check that intersect falls on both lines
prop_line_line2_intersection_on_both :: Line R2 -> Line R2 -> Result
prop_line_line2_intersection_on_both u@(Line {lPoint=up, lDir=ud}) v@(Line {lPoint=vp, lDir=vd})
        | vd `parallel` ud     = rejected
        | otherwise = case lineLine2Intersect u v of
                IIntersect i  -> let t  = ((i .-. up) <.> ud) / magnitudeSq ud
                                     t' = ((i .-. vp) <.> vd) / magnitudeSq vd
                                 in liftBool $ magnitude ((up .+^ ud^*t ) .-. i) < 1e-8
                                            && magnitude ((vp .+^ vd^*t') .-. i) < 1e-8

-- Properties for faceLineIntersect
-- | Check that face-line intersections are found
prop_face_line_intersection_hit :: Face -> NormalizedV NR3 -> Normalized Double -> Result
prop_face_line_intersection_hit face@(Face {faceVertices=(v0,v1,v2)}) (NormalizedV dir) (Normalized a)
        | magnitude ((v1.-.v0) `cross3` (v2.-.v0)) =~ 0 = rejected
        | otherwise = 
                let tol = 1e-8
                    u = v1 .-. v0
                    v = v2 .-. v0
                    a' = (1-tol) * a -- Avoid numerical error near edges
                    b' = (1-tol) - a'
                    hitPoint = v0 .+^ a' *^ u + b' *^ v
                    origin = hitPoint .-^ dir
                in case faceLineIntersect face (Line {lPoint=origin, lDir=dir}) of
                        IIntersect intersect  -> liftBool $ magnitude (intersect .-. hitPoint) < 1e-5
                        INull                 -> failed {reason="No intersection found"}

-- | Check that only intersections are found
prop_face_line_intersection_miss :: Face -> NormalizedV NR3 -> Normalized Double -> Result
prop_face_line_intersection_miss face@(Face {faceVertices=(v0,v1,v2)}) (NormalizedV dir) (Normalized a) =
        let tol = 1e-8
            u = v1 .-. v0
            v = v2 .-. v0
            a' = (1-tol) * a
            b' = (1-tol) * a'
            hitPoint = v0 .-^ a' *^ u - b' *^ v
            origin = hitPoint .-^ dir
        in case faceLineIntersect face (Line {lPoint=origin, lDir=dir}) of
                IIntersect intersect  -> failed {reason="Found non-existent intersection"}
                INull                 -> succeeded

-- Properties for planeLineIntersect
-- | Check that line-plane intersection point falls on plane
prop_plane_line_intersection_on_plane :: Line R3 -> Plane R3 -> Bool
prop_plane_line_intersection_on_plane line plane@(Plane n (P p)) =
        abs ((i - p) <.> n) < 1e-8
        where IIntersect (P i) = planeLineIntersect plane line

-- | Check that line-plane intersection point falls on line
prop_plane_line_intersection_on_line :: Line R3 -> Plane R3 -> Bool
prop_plane_line_intersection_on_line line@(Line {lPoint=P v, lDir=d}) plane = 
        magnitude (lambda *^ d + v - p) < 1e-8
        where IIntersect (P p) = planeLineIntersect plane line
              lambda = (p - v) <.> d ^/ magnitudeSq d

-- | Check that line falling on plane is degenerate
prop_plane_line_intersection_degenerate :: Plane R3 -> R3 -> NonNull R3 -> Bool
prop_plane_line_intersection_degenerate plane@(Plane {planeNormal=n, planePoint=p}) a (NonNull b) =
        case planeLineIntersect plane line of
             IDegenerate        -> True
             otherwise          -> False
        where b' = b - n ^* (n <.> b)
              line = Line {lPoint=p .+^ a `cross3` n, lDir=normalized b'}

-- Properties for pointOnPlane
-- | Check that point on plane is recognized
prop_point_on_plane_hit :: Plane R3 -> R3 -> Bool
prop_point_on_plane_hit plane v =
        pointOnPlane plane (planePoint plane .+^ v')
        where n = planeNormal plane
              v' = v - n ^* (v <.> n)

-- | Check that point off plane is recognized
prop_point_on_plane_miss :: Plane R3 -> NonNull R3 -> Result
prop_point_on_plane_miss plane@(Plane {planeNormal=n}) (NonNull v) 
        | abs (n <.> v) < 1e-8  = rejected
        | otherwise             = liftBool $ not $ pointOnPlane plane (P v)

-- Properties for planeFaceIntersect
-- | Check that a face sitting in the plane is recognized as degenerate
prop_plane_face_intersect_face_in_plane :: Plane R3 -> NonNull R3 -> NonNull R3 -> NonNull R3 -> Bool
prop_plane_face_intersect_face_in_plane plane (NonNull a) (NonNull b) (NonNull c) =
        case planeFaceIntersect plane face of
             IDegenerate        -> True
             otherwise          -> False
        where Plane {planePoint=p, planeNormal=n} = plane
              proj x = x - n ^* (n <.> x) -- Project onto plane
              vs = (p .+^ proj a, p .+^ proj b, p .+^ proj c)
              face = faceFromVertices vs

-- | Check that a face intersection with a plane gives a line segment
prop_plane_face_intersect_face_intersects_plane :: Plane R3 -> NonZero R3 -> NonZero R3 -> NonZero R3 -> Result
prop_plane_face_intersect_face_intersects_plane plane (NonZero a) (NonZero b) (NonZero c)
        | parallel (faceNormal face) (planeNormal plane)  = rejected
        | otherwise  =
                case planeFaceIntersect plane face of
                     IIntersect _       -> succeeded
                     otherwise          -> failed
        where d = planePoint plane .-^ (a + b + c) ^/ 3   -- Displacement from center of points to planePoint
              face = faceFromVertices (d.+^a, d.+^b, d.+^c)

-- | Check face with one edge sitting on plane
prop_plane_face_intersect_edge_on_plane :: Plane R3 -> NonZero R3 -> NonZero R3 -> NonNull R3 -> Result
prop_plane_face_intersect_edge_on_plane plane (NonZero a) (NonZero b) (NonNull c) =
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
              a' = p .+^ projInPlane plane a
              b' = p .+^ projInPlane plane b
              c' = p .+^ c
              face = faceFromVertices (a', b', c')

-- Properties for planeLineSegIntersect
-- | Check that line sitting in plane is degenerate
prop_plane_line_seg_intersect_line_in_plane :: Plane R3 -> NonZero R3 -> NonZero R3 -> Bool
prop_plane_line_seg_intersect_line_in_plane plane (NonZero a) (NonZero b) =
        case planeLineSegIntersect plane l of
             IDegenerate        -> True
             otherwise          -> False
        where Plane {planePoint=p, planeNormal=n} = plane
              proj = projInPlane plane
              l = LineSeg (p .+^ proj a) (p .+^ proj b)

-- | Check that non-degenerate plane-line segment intersections work
prop_plane_line_seg_intersect_hit :: Plane R3 -> NonZero R3 -> Result
prop_plane_line_seg_intersect_hit plane (NonZero a)
        | perpendicular (planeNormal plane) a  = rejected
        | otherwise =
                case planeLineSegIntersect plane l of
                     IIntersect i  -> if magnitude (i.-.p) < 1e-8 then succeeded
                                                                else failed {reason="Incorrect intersection"}
                     otherwise     -> failed {reason="No intersection found"}
        where p = planePoint plane
              l = LineSeg (p .+^ a) (p .-^ a)

-- | Check that line parallel to plane sitting out of plane doesn't intersect
prop_plane_line_seg_intersect_parallel_off_plane :: Plane R3 -> NonZero Double -> R3 -> NonZero R3 -> Result
prop_plane_line_seg_intersect_parallel_off_plane plane (NonZero s) a (NonZero b) =
        case planeLineSegIntersect plane l of
             INull        -> succeeded
             otherwise    -> failed
        where Plane {planeNormal=n, planePoint=p} = plane
              a' = (p .+^ n ^* s) .+^ projInPlane plane a
              b' = a' .+^ projInPlane plane b
              l = LineSeg a' b'

runTests = $quickCheckAll

