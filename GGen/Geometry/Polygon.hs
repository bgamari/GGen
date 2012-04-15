{-# LANGUAGE TupleSections, FlexibleContexts, TemplateHaskell #-}

module GGen.Geometry.Polygon ( lineSegPaths
                             , lineSegPathToPolygon
                             , lineSegsToPolygons
                             , polygonToLineSegPath
                             , linePolygon2Crossings
                             , fixPolygon2Chirality
                             , offsetPolygon
                             , runTests
                             ) where

import System.IO.Unsafe (unsafePerformIO)
import GGen.Render
import qualified GGen.Pretty as P
import Text.PrettyPrint.HughesPJ (($$), (<+>))

import Data.Maybe (maybe)
import Data.Either (partitionEithers)
import Data.List (sortBy, delete, deleteFirstsBy, foldl')
import Data.Maybe (fromJust, mapMaybe, catMaybes, listToMaybe)
import Data.VectorSpace
import Data.AffineSpace

import GGen.Geometry.Types hiding (runTests)
import GGen.Geometry.Intersect hiding (runTests)

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
        let dist l = magnitude $ lsA p .-. lsB l
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

-- | Order the points of a polygon in clockwise order
fixPolygon2Chirality :: Polygon R2 -> Polygon R2
fixPolygon2Chirality poly@(Polygon points)
        | length points < 3 = error $ "Polygons must have at least three points: "++show poly
        | cross > 1 = poly
        | otherwise = Polygon $ reverse points
        where a:b:c:_ = points
              (ux,uy) = unr2 $ a .-. b
              (vx,vy) = unr2 $ c .-. b
              cross = ux*vy - uy*vx

-- | Find points of intersection between a line and polygon where the line
-- actually crosses the polygon's boundary. This eliminates cases where
-- the line skims a corner by testing that the dot product of consecutive
-- edges' normals and the line are of the same sign
linePolygon2Crossings :: Line R2 -> Polygon R2 -> [P2]
linePolygon2Crossings l@(Line {lDir=dir}) poly =
        let f :: [LineSeg R2] -> [P2]
            f ls@(a:b:c:_) = 
                let an = ls2Normal a LeftHanded <.> dir
                    bn = ls2Normal b LeftHanded <.> dir
                    cn = ls2Normal c LeftHanded <.> dir
                in case (lineLineSeg2Intersect l a, lineLineSeg2Intersect l b) of
                     (IIntersect ia, IIntersect ib)  | ia `coincident` ib && an * bn >= 0  ->
                             -- The line is crossing through a vertex
                             ia : f (tail ls)
                     (IIntersect ia, IIntersect ib)  | ia `coincident` ib && an * bn < 0  ->
                             -- The line is grazing a vertex
                             f (tail ls)
                     (IIntersect ia, IDegenerate)  | an * cn > 0  ->
                             -- The line is degenerate to an edge yet still crosses
                             alerp (lsA b) (lsB b) 0.5 : f (tail ls)
                     (_, IIntersect ib)  | not (ib `coincident` lsA b) && not (ib `coincident` lsB b)  ->
                             -- The line is crossing through an edge
                             ib : f (tail ls)
                     otherwise  ->
                             -- The line isn't crossing
                             f (tail ls)
            f (_:_:[]) = []
            segs = polygonToLineSegPath poly
        in nubPoints $ f (segs ++ take 2 segs)

-- | Offset polygon boundaries inward or outwards
-- Positive offset is outwards
offsetPolygon :: Double -> Polygon R2 -> Polygon R2
offsetPolygon offset = Polygon . f . polygonToLineSegPath
        where f :: LineSegPath R2 -> [P2]
              f segs@(s:s':_) = 
                let p = lsB s
                    l  = Line (p .+^ ls2Normal s RightHanded)  (offset *^ normalized (lsDispl s))
                    l' = Line (p .+^ ls2Normal s' RightHanded) (offset *^ normalized (lsDispl s'))
                    IIntersect p' = lineLine2Intersect l l'
                in p':f (tail segs)

-- QuickCheck properties

-- Properties for polygon-line segment conversion
prop_polygon_line_seg_roundtrip :: Polygon R2 -> Result
prop_polygon_line_seg_roundtrip poly@(Polygon points)
        | length points < 3 = rejected
        | otherwise = let ls = polygonToLineSegPath poly
                      in case lineSegPathToPolygon ls of
                              Just poly' -> liftBool $ poly == poly'
                              Nothing    -> failed {reason="No polygon found"}

prop_line_seg_polygon_roundtrip :: [P2] -> Result
prop_line_seg_polygon_roundtrip points
        | length points < 3 = rejected
        | otherwise = let ls = polygonToLineSegPath (Polygon points)
                          poly = fromJust $ lineSegPathToPolygon ls
                          ls' = polygonToLineSegPath poly
                      in liftBool $ ls `approx` ls'

-- Properties for linePolygon2Crossings
--prop_line_polygon2_crossings_corner_miss :: Polygon P2 -> Result
--prop_line_polygon2_crossings_corner_miss poly@(Polygon points) =
--        let l = Line points

runTests = $quickCheckAll

