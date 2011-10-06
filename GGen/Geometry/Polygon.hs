{-# LANGUAGE TupleSections #-}

module GGen.Geometry.Polygon ( lineSegPaths
                             , lineSegsToPolygons
                             , polygonToLineSegs
                             , planeSlice
                             , rayLineSegPathIntersects
                             , OrientedPolygon
                             ) where

import Debug.Trace
import qualified GGen.Pretty as P
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (($$), (<+>))

import Data.List (sortBy, delete, (\\), foldl')
import Data.Maybe (fromJust, mapMaybe, catMaybes, listToMaybe)
import Data.VectorSpace
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
        let dist l = magnitude $ lsBegin p ^-^ lsEnd l
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
        | not $ begin `coincident` end  = Nothing
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
rayLineSegPathIntersects ray =
        mapMaybe (f ray)
        where f r l  | IIntersect a <- i   = Just a
                     | otherwise           = Nothing
                     where i = rayLineSegIntersect r l

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
            boundaryMap = mapIntersection findFaceIntersect faces -- Map from line segments to faces
            -- Make sure we include flipped line segments in map
            boundaryMap' = boundaryMap ++ map (\(l,face) -> (invertLineSeg l, face)) boundaryMap
            paths = lineSegPaths $ map fst boundaryMap

            -- | Figure out whether polygon should be filled
            orientPath :: LineSegPath -> OrientedPolygon
            orientPath path = let l = head path
                                  face = maybe (error $ "Can't find face for line"++show l) id
                                       $ lookup l boundaryMap'
                                  origin = lerp (lsBegin l) (lsEnd l) 0.5
                                  normal = planeFaceNormal plane face
                                  intersects = rayLineSegPathIntersects (Ray origin normal) path
                                  hi = PP.text "origin" <+> P.point origin
                                    $$ PP.text "normal" <+> P.vec normal
                                    $$ PP.text "intersects" <+> (PP.vcat $ map P.point intersects)
                                    $$ PP.text ""
                                  fill = trace (show hi)
                                       $ length intersects `mod` 2 == 1
                              in (fromJust $ lineSegPathToPolygon path, fill)
        in map orientPath paths

