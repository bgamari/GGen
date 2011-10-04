{-# LANGUAGE TupleSections #-}

module GGen.Geometry.Polygon ( lineSegPaths
                             , lineSegsToPolygons
                             , polygonToLineSegs
                             , planeSlice
                             , rayLineSegPathIntersects
                             , OrientedPolygon
                             ) where

import Data.List (sortBy, delete, (\\), foldl')
import Data.Maybe (fromJust, mapMaybe, catMaybes, listToMaybe)
import Numeric.LinearAlgebra
import GGen.Geometry.Types
import GGen.Geometry.LineSeg (invertLineSeg)
import GGen.Geometry.Intersect (rayLineSegIntersect, planeFaceIntersect, planeFaceNormal)

-- | Find contiguous paths of line segments
lineSegPaths :: [LineSeg] -> [LineSegPath]
lineSegPaths [] = []
lineSegPaths ls = let (p,rest) = lineSegPath' ls [] True
                  in p : lineSegPaths rest

lineSegPath' :: [LineSeg] -> LineSegPath -> Bool -> (LineSegPath, [LineSeg])
lineSegPath' (l:ls) [] _ = lineSegPath' ls [l] True
lineSegPath' ls path@(p:_) canFlip =
        let dist l = norm2 $ lsBegin p - lsEnd l
            next   = listToMaybe
                   $ sortBy (\l l' -> compare (dist l) (dist l'))
                   $ filter (\l -> dist l < pointTol) ls
        in case next of
                Just l  -> lineSegPath' (ls \\ [l]) (l:path) True
                Nothing -> if canFlip then lineSegPath' (map invertLineSeg ls) path False
                                      else (path, ls)

-- | Find the polygon representing the given line segment path
lineSegPathToPolygon :: LineSegPath -> Maybe Polygon
lineSegPathToPolygon path
        | not $ samePoint begin end     = Nothing
        | otherwise                     = Just $ f path
        where begin = lsBegin $ head path
              end = lsEnd $ last path
              f (p:[]) = [lsBegin p, lsEnd p]
              f (p:path) = lsBegin p : f path

-- | Try to match up a set of line segments into a closed polygon
lineSegsToPolygons :: [LineSeg] -> [Polygon]
lineSegsToPolygons = map (fromJust . lineSegPathToPolygon) . lineSegPaths

-- | (poly, True) refers to a polygon poly which should have its interior filled
type OrientedPolygon = (Polygon, Bool)

-- | Points of intersection between a ray and a polygon
rayLineSegPathIntersects :: Ray -> LineSegPath -> [Point]
rayLineSegPathIntersects ray = mapMaybe (rayLineSegIntersect ray)

-- | Get line segments of polygon boundary
polygonToLineSegs :: Polygon -> [LineSeg]
polygonToLineSegs (_:[]) = []
polygonToLineSegs poly@(a:b:_) = (LineSeg a b) : (polygonToLineSegs $ tail poly)

-- | Try to find the boundaries sitting in a plane
-- In order to identify the interior of each polygon, we build a map from line
-- segment endpoints to their corresponding faces so we can later find the normals. 
planeSlice :: Plane -> [Face] -> [OrientedPolygon]
planeSlice plane faces =
        let findFaceIntersect face = planeFaceIntersect plane face >>= (return . (,face))
            boundaryMap = mapMaybe findFaceIntersect faces -- Map from line segments to faces
            paths = lineSegPaths $ map fst boundaryMap
            orientPath :: LineSegPath -> OrientedPolygon
            orientPath path = let l = head path
                                  face = fromJust $ lookup l boundaryMap
                                  origin = lsBegin l + 0.5 `scale` (lsEnd l - lsBegin l)
                                  normal = planeFaceNormal plane face
                                  intersects = length $ rayLineSegPathIntersects (Ray origin normal) path
                                  fill = intersects `mod` 2 == 1
                              in (fromJust $ lineSegPathToPolygon path, fill)
        in map orientPath paths

