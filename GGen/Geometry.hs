module GGen.Geometry where

import Data.Maybe (mapMaybe, listToMaybe)
import Data.List (sortBy, delete)
import Control.Monad (guard)
import Numeric.LinearAlgebra
import GGen.Types

-- | The maximum distance between identical points
pointTol = 1e-4

-- | The maximum deviation from one in a dot product to consider vectors parallel
dirTol = 1e-4

-- | Test whether a point sits on a face
-- Based upon http://softsurfer.com/Archive/algorithm_0105/algorithm_0105.htm#Segment-Triangle
pointOnFace :: Face -> Point -> Bool
pointOnFace face p =
        let (v0,v1,v2) = faceVertices face
            u = v1 - v0
            v = v2 - v0
            w = p - v0
            uu = u `dot` u
            vv = v `dot` v
            uv = u `dot` v
            wv = w `dot` v
            wu = w `dot` u
            s = (uv*wv - vv*wu) / (uv^2 - uu*vv)
            t = (uv*wu - uu*wv) / (uv^2 - uu*vv)
        in s >= 0 && t >= 0 && s+t <= 1

-- | Point of intersection between a face and a line
faceLineIntersect :: Face -> Line -> Maybe Point
faceLineIntersect face@(Face {faceNormal=n, faceVertices=(v,_,_)}) (Line (a,m))
        | pointOnFace face p    = Just p
        | otherwise             = Nothing
        where lambda = (n `dot` (v-a)) / (n `dot` m)
              p = a + lambda `scale` m

-- | Point of intersection between plane and line segment
planeLineSegIntersect :: Plane -> LineSeg -> Maybe Point
planeLineSegIntersect (Plane {normal=n, point=v}) (LineSeg (a,b))
        | lambda < 0 = Nothing
        | lambda > 1 = Nothing
        | otherwise  = Just $ a + lambda `scale` (b-a)
        where lambda = (n `dot` (v-a)) / (n `dot` (b-a))

-- | Line segment of intersection between plane and face
planeFaceIntersect :: Plane -> Face -> Maybe LineSeg
planeFaceIntersect plane (Face {faceVertices=(a,b,c)}) =
        let lines = map LineSeg [(a,b), (b,c), (c,a)]
            lineIntersects = mapMaybe (planeLineSegIntersect plane) lines
        in case length lineIntersects of
                0         -> Nothing
                1         -> error "Only one intersection point"
                2         -> Just $ LineSeg (head lineIntersects, last lineIntersects)
                otherwise -> error "Unexpected number of intersections"

-- | A group of polygons marked with whether their interiors should be filled
data BodySlice = BodySlice { bsPolys :: [(Polygon, Bool)] }
                           deriving (Show)

-- | Try to match up a set of line segments into a closed polygon
lineSegsToPolygon :: [LineSeg] -> Maybe Polygon
lineSegsToPolygon [] = Just []
lineSegsToPolygon (LineSeg (a,_):segs) = lineSegsToPolygon' [a] segs

lineSegsToPolygon' :: Polygon -> [LineSeg] -> Maybe Polygon
lineSegsToPolygon' poly []
        | dist < pointTol  = Just poly
        | otherwise        = Nothing
        where start = last poly
              end = head poly
              dist = norm2 (end-start)

lineSegsToPolygon' poly@(p:_) segs =
        do let targets = segs ++ map invertLineSeg segs -- List of possible next line segments
               distanceAndTarget seg@(LineSeg (x,_)) = (norm2 (p-x), seg)
               distTargets = sortBy (\(da,_) (db,_) -> compare da db)
                           $ filter ((<pointTol).fst)
                           $ map distanceAndTarget targets
           guard (not $ null distTargets)
           let target@(LineSeg (a,b)) = snd $ head distTargets
           lineSegsToPolygon' (b:a:poly) (delete target segs)

-- | Reverse the order of line segment termini
invertLineSeg :: LineSeg -> LineSeg
invertLineSeg (LineSeg (a,b)) = LineSeg (b,a)

-- | Displacement of a line segment
lineSegDispl :: LineSeg -> Vec
lineSegDispl (LineSeg (a,b)) = b-a

-- | Are two points the same to within pointTol?
samePoint a b = norm2 (a-b) < pointTol

-- | Try merging two line segments
tryMergeLineSegs :: LineSeg -> LineSeg -> Maybe LineSeg
tryMergeLineSegs a b =
        let perms = [(a,b),
                     (invertLineSeg a, b), (a, invertLineSeg b),
                     (invertLineSeg a, invertLineSeg b)]
            f (LineSeg (a1,a2), LineSeg (b1,b2)) =
                    if a1 `samePoint` b1 then Just $ LineSeg (a2,b2)
                                         else Nothing
            dirDev = abs (lineSegDispl a `dot` lineSegDispl b) - 1
            merged = mapMaybe f perms
        in if dirDev < dirTol && (not $ null merged) then Just $ head merged
                                                     else Nothing
-- | Merging two line segments if possible
mergeLineSegs :: LineSeg -> LineSeg -> [LineSeg]
mergeLineSegs a b = maybe [a,b] (replicate 1) $ tryMergeLineSegs a b

-- | List version of mergeLineSegs
mergeLineSegs' :: [LineSeg] -> [LineSeg]
mergeLineSegs' ls = let tryMerge done [] = done
                        tryMerge done (l:ls) =
                                let f l' = case tryMergeLineSegs l l' of
                                                    Just try -> Just (try, delete l' ls)
                                                    Nothing  -> Nothing
                                    merged = mapMaybe f ls
                                in if null merged then tryMerge (l:done) ls
                                                  else let (m,ls') = head merged
                                                       in  tryMerge (m:done) ls'
                    in tryMerge [] ls

-- | Try to find the boundaries sitting in a plane
planeSlice :: Plane -> [Face] -> Maybe Polygon
planeSlice plane faces = let boundaries = mergeLineSegs' $ mapMaybe (planeFaceIntersect plane) faces
                         in lineSegsToPolygon boundaries

