{-# LANGUAGE TupleSections, FlexibleContexts, TemplateHaskell #-}

module GGen.Geometry.Polygon ( lineSegPaths
                             , lineSegPathToPolygon
                             , lineSegsToPolygons
                             , polygonToLineSegPath
                             , planeSlice
                             , linePolygon2Crossings
                             , runTests
                             ) where

import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)
import GGen.Render
import qualified GGen.Pretty as P
import Text.PrettyPrint.HughesPJ (($$), (<+>))

import Data.Maybe (maybe)
import Data.Either (partitionEithers)
import Data.List (sortBy, delete, deleteFirstsBy, foldl')
import Data.Maybe (fromJust, mapMaybe, catMaybes, listToMaybe)
import Data.VectorSpace

import GGen.Geometry.Types hiding (runTests)
import GGen.Geometry.Intersect (lineSegLineSeg2Intersect, planeFaceIntersect, lineLineSeg2Intersect)
import GGen.Geometry.BoundingBox (facesBoundingBox)
import GGen.Geometry.LineSeg (mergeLineSegList)

import Test.QuickCheck.All
import Test.QuickCheck.Property

-- | Find contiguous paths of line segments
lineSegPaths :: (InnerSpace p, RealFloat (Scalar p), Eq p) => [LineSeg p] -> [LineSegPath p]
lineSegPaths [] = []
lineSegPaths ls = let (p,rest) = lineSegPath' ls [] True
                  in p : lineSegPaths rest

lineSegPath' :: (InnerSpace p, RealFloat (Scalar p), Eq p) => [LineSeg p] -> LineSegPath p -> Bool -> (LineSegPath p, [LineSeg p])
lineSegPath' (l:ls) [] _ = lineSegPath' ls [l] True
lineSegPath' ls path@(p:_) canFlip =
        let dist l = magnitude $ lsA p ^-^ lsB l
            next   = listToMaybe
                   $ sortBy (\l l' -> compare (dist l) (dist l'))
                   $ filter (\l -> dist l < realToFrac pointTol) ls
        in case next of
                Just l  -> lineSegPath' (deleteFirstsBy approx ls [l]) (l:path) True
                Nothing -> if canFlip then lineSegPath' (map lsInvert ls) path False
                                      else (path, ls)

-- | Find the polygon representing the given line segment path
lineSegPathToPolygon :: (InnerSpace p, RealFloat (Scalar p)) => LineSegPath p -> Maybe (Polygon p)
lineSegPathToPolygon path
        | length path < 3               = Nothing
        | not $ begin `coincident` end  = Nothing
        | otherwise                     = Just $ Polygon $ f path
        where begin = lsA $ head path
              end = lsB $ last path
              f [] = []
              f (p:path) = lsA p : f path

-- | Try to match up a set of line segments into a set of closed polygons
-- Returns tuple with resulting polygons and line segment paths which could not
-- be closed
lineSegsToPolygons :: (InnerSpace p, RealFloat (Scalar p), Eq p) => [LineSeg p] -> ([Polygon p], [LineSegPath p])
lineSegsToPolygons = partitionEithers . map f . lineSegPaths
        where f path = maybe (Right path) Left $ lineSegPathToPolygon path

-- | Get line segments of polygon boundary
polygonToLineSegPath :: (InnerSpace p, RealFloat (Scalar p)) => Polygon p -> LineSegPath p
polygonToLineSegPath (Polygon points)
        | length points < 3  = error "Polygons must have at least 4 points"
        | otherwise          = f points
        where f (a:[]) = [LineSeg a (head points)]
              f points@(a:b:_) = (LineSeg a b) : (f $ tail points)

-- | Try to find the boundaries sitting in a plane
-- Assumes slice is in XY plane
planeSlice :: Plane Point -> [Face] -> [OrientedPolygon Point2]
planeSlice plane faces =
        let proj (x,y,_) = (x,y)  -- | Project vector to XY plane
            projPolygon = map proj
            projLineSeg (LineSeg a b) = LineSeg (proj a) (proj b)  -- | Project line segment to XY plane
            projLineSegPath = map projLineSeg

            lines = mapIntersectionDropDegen (planeFaceIntersect plane) faces 
            paths = lineSegPaths $ mergeLineSegList $ map projLineSeg lines
            polys = mapMaybe lineSegPathToPolygon paths
            pps = mapMaybe (\path->do poly <- lineSegPathToPolygon path
                                      return (poly,path)
                           ) paths

            -- To figure out filled-ness, we project a segment from outside of the bounding box to each
            -- of the line segment paths, counting intersections as we go
            (bbMin, bbMax) = facesBoundingBox faces
            origin = proj $ bbMax + (bbMax-bbMin) ^* 0.1

            -- | Figure out whether polygon should be filled
            fillPoly :: LineSegPath Point2 -> Bool
            fillPoly path = let 
                                LineSeg a b = head path
                                ll = LineSeg origin $ lerp a b 0.5
                                inters = mapIntersection (lineSegLineSeg2Intersect ll)
                                       $ concat (deleteFirstsBy approx paths [path])
                            in length inters `mod` 2 == 1

        in map (\poly->orientPolygon2 poly (fillPoly (polygonToLineSegPath poly))) polys

fixPolygon2Chirality :: Polygon Point2 -> Polygon Point2
fixPolygon2Chirality poly@(Polygon points)
        | length points < 3 = error $ "Polygons must have at least three points: "++show poly
        | cross > 1 = poly
        | otherwise = Polygon $ reverse points
        where a:b:c:_ = points
              (ux,uy) = a - b
              (vx,vy) = c - b
              cross = ux*vy - uy*vx

orientPolygon2 :: Polygon Point2 -> Bool -> OrientedPolygon Point2
orientPolygon2 poly fill
        | fill          = (poly', RightHanded)
        | otherwise     = (poly', LeftHanded)
        where poly' = fixPolygon2Chirality poly

-- | Find points of intersection between a line and polygon where the line
-- actually crosses the polygon's boundary. This eliminates cases where
-- the line skims a corner by testing that the dot product of consecutive edges
-- normals and the line are of the same sign
linePolygon2Crossings :: Line Point2 -> Polygon Point2 -> [Point2]
linePolygon2Crossings l@(Line {lDir=dir}) poly =
        let f :: [LineSeg Point2] -> [Point2]
            f ls@(a:b:_) = 
                let an = ls2Normal a LeftHanded <.> dir
                    bn = ls2Normal b LeftHanded <.> dir
                in case (lineLineSeg2Intersect l a, lineLineSeg2Intersect l b) of
                     (IIntersect ia, IIntersect ib)  | an * bn > 0  -> ia : f (tail ls)
                     (IIntersect ia, _)                             -> ia : f (tail ls)
                     otherwise                                      -> f (tail ls)
            f (_:[]) = []
            segs = polygonToLineSegPath poly
        in nubPoints $ f (segs ++ [head segs])


-- QuickCheck properties

-- Properties for polygon-line segment conversion
prop_polygon_line_seg_roundtrip :: Polygon Point2 -> Result
prop_polygon_line_seg_roundtrip poly@(Polygon points)
        | length points < 3 = rejected
        | otherwise = let ls = polygonToLineSegPath poly
                      in case lineSegPathToPolygon ls of
                              Just poly' -> liftBool $ poly == poly'
                              Nothing    -> failed {reason="No polygon found"}

prop_line_seg_polygon_roundtrip :: [Point2] -> Result
prop_line_seg_polygon_roundtrip points
        | length points < 3 = rejected
        | otherwise = let ls = polygonToLineSegPath (Polygon points)
                          poly = fromJust $ lineSegPathToPolygon ls
                          ls' = polygonToLineSegPath poly
                      in liftBool $ ls `approx` ls'

-- Properties for linePolygon2Crossings
-- TODO

runTests = $quickCheckAll

