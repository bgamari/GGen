module GGen.Slice ( planeSlice
                  ) where

import Data.VectorSpace
import Data.AffineSpace
import Data.List (deleteFirstsBy, partition)

import GGen.Types
import GGen.Geometry.Types
import GGen.Geometry.Polygon (lineSegPaths, lineSegsToPolygons, polygonToLineSegPath, fixPolygon2Chirality)
import GGen.Geometry.Intersect (planeFaceIntersect, lineSegLineSeg2Intersect)
import GGen.Geometry.LineSeg (mergeLineSegList)
import GGen.Geometry.BoundingBox (facesBoundingBox)

orientPolygon :: Polygon R2 -> Bool -> OrientedPolygon R2
orientPolygon poly fill
        | fill          = (poly', RightHanded)
        | otherwise     = (poly', LeftHanded)
        where poly' = fixPolygon2Chirality poly

unitZ = r3 (0,0,1)
        
-- | Try to find the boundaries sitting in a plane
-- Assumes slice is in XY plane
planeSlice :: [Face] -> Double -> Double -> Slice
planeSlice faces height z =
        let plane = Plane { planeNormal=unitZ, planePoint=bbMin .+^ unitZ ^* z }
            proj p = let (x,y,_) = unp3 p
                     in p2 (x,y)  -- | Project point onto XY plane
            projPolygon = map proj
            projLineSeg (LineSeg a b) = LineSeg (proj a) (proj b)  -- | Project line segment to XY plane
            projLineSegPath = map projLineSeg
            (_,_,planeZ) = unp3 $ planePoint plane

            inPlane face = (abs (z - planeZ) < height) && (faceNormal face `parallel` unitZ)
                           where (_,_,z) = unp3 p
                                 (p,_,_) = faceVertices face
            (inPlaneFaces, outPlaneFaces) = partition inPlane faces
            lines :: [LineSeg R3]
            lines = mergeLineSegList
                  $ mapIntersection (planeFaceIntersect plane)
                  $ outPlaneFaces
            paths = map projLineSegPath $ lineSegPaths lines
            (polys, unmatchedPaths) = lineSegsToPolygons $ map projLineSeg lines

            -- To figure out filled-ness, we project a segment from outside of the bounding box to each
            -- of the line segment paths, counting intersections as we go
            (bbMin, bbMax) = facesBoundingBox faces
            origin = proj $ bbMax .+^ (bbMax.-.bbMin) ^* 0.1

            -- | Figure out whether polygon should be filled
            fillPoly :: Polygon R2 -> Bool
            fillPoly poly = let path = polygonToLineSegPath poly
                                LineSeg a b = head path
                                ll = LineSeg origin $ alerp a b 0.5
                                inters = mapIntersection (lineSegLineSeg2Intersect ll)
                                       $ concat (deleteFirstsBy approx paths [path])
                            in length inters `mod` 2 == 1

            -- | Figure out whether a polygon is exposed. We see whether any of
            -- the polygon vertices are coincident with a vertex of an exposed
            -- face
            exposedVerts = map proj 
                         $ concat
                         $ map (\(Face _ (a,b,c)) -> [a,b,c])
                         $ inPlaneFaces
            polyExposed :: Polygon R2 -> Exposure
            polyExposed (Polygon vs) =
                    let checkVert v = or $ map (`coincident` v) exposedVerts
                    in case or $ map checkVert vs of
                            True  -> External
                            False -> Internal

        in (z, map (\poly -> (orientPolygon poly (fillPoly poly), polyExposed poly)) polys)

