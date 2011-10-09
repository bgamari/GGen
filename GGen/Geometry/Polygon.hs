{-# LANGUAGE TupleSections, FlexibleContexts #-}

module GGen.Geometry.Polygon ( lineSegPaths
                             , lineSegPathToPolygon
                             , lineSegsToPolygons
                             , polygonToLineSegs
                             , planeSlice
                             , OrientedPolygon
                             ) where

import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)
import GGen.Render
import qualified GGen.Pretty as P
import Text.PrettyPrint.HughesPJ (($$), (<+>))

import Data.Maybe (maybe)
import Data.Either (partitionEithers)
import Data.List (sortBy, delete, (\\), foldl')
import Data.Maybe (fromJust, mapMaybe, catMaybes, listToMaybe)
import Data.VectorSpace

import GGen.Geometry.Types
import GGen.Geometry.Intersect (lineSegLineSeg2Intersect, planeFaceIntersect)
import GGen.Geometry.BoundingBox (facesBoundingBox)
import GGen.Geometry.LineSeg (mergeLineSegList)

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
                Just l  -> lineSegPath' (ls \\ [l]) (l:path) True
                Nothing -> if canFlip then lineSegPath' (map lsInvert ls) path False
                                      else (path, ls)

-- | Find the polygon representing the given line segment path
lineSegPathToPolygon :: (InnerSpace p, RealFloat (Scalar p)) => LineSegPath p -> Maybe (Polygon p)
lineSegPathToPolygon path
        | length path < 3               = Nothing
        | not $ begin `coincident` end  = Nothing
        | otherwise                     = Just $ f path
        where begin = lsA $ head path
              end = lsB $ last path
              f (p:[]) = [lsA p, lsB p]
              f (p:path) = lsA p : f path

-- | Try to match up a set of line segments into a closed polygon
-- Returns tuple with resulting polygons and unassigned line segments
lineSegsToPolygons :: (InnerSpace p, RealFloat (Scalar p), Eq p) => [LineSeg p] -> ([Polygon p], [LineSegPath p])
lineSegsToPolygons = partitionEithers . map f . lineSegPaths
        where f path = maybe (Right path) Left $ lineSegPathToPolygon path

-- | Get line segments of polygon boundary
polygonToLineSegs :: (InnerSpace p, RealFloat (Scalar p)) => Polygon p -> [LineSeg p]
polygonToLineSegs (_:[]) = []
polygonToLineSegs poly@(a:b:_) = (LineSeg a b) : (polygonToLineSegs $ tail poly)

-- | Try to find the boundaries sitting in a plane
-- Assumes slice is in XY plane
planeSlice :: Plane Point -> [Face] -> [OrientedPolygon Point2]
planeSlice plane faces =
        let proj (x,y,_) = (x,y)  -- | Project vector to XY plane
            projPolygon = map proj
            projLineSeg (LineSeg a b) = LineSeg (proj a) (proj b)  -- | Project line segment to XY plane
            projLineSegPath = map projLineSeg

            f :: Face -> Maybe (LineSeg Point)
            f face = case planeFaceIntersect plane face of
                IIntersect i  -> Just i
                INull         -> Nothing
                IDegenerate   -> Nothing  -- TODO: Figure this out
            lines = mergeLineSegList $ mapMaybe f faces
            paths :: [LineSegPath Point2]
            paths = map projLineSegPath $ lineSegPaths lines

            -- To figure out filled-ness, we project a segment from outside of the bounding box to each
            -- of the line segment paths, counting intersections as we go
            (bbMin, bbMax) = facesBoundingBox faces
            origin = proj $ bbMax + (bbMax-bbMin) ^* 0.1

            -- | Figure out whether polygon should be filled
            orientPath :: LineSegPath Point2 -> OrientedPolygon Point2
            orientPath path = let (LineSeg a b) = head path
                                  target = lerp a b 0.5
                                  ll = LineSeg origin target
                                  segments = concat (paths \\ [path])
                                  intersects = mapIntersectionDropDegen (lineSegLineSeg2Intersect ll) segments
                                  fill = length intersects `mod` 2 == 1
                              in maybe (error "WTF") (,fill) $ lineSegPathToPolygon path
        in map orientPath paths

