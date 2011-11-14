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

orientPolygon :: Polygon Vec2 -> Bool -> OrientedPolygon Vec2
orientPolygon poly fill
        | fill          = (poly', RightHanded)
        | otherwise     = (poly', LeftHanded)
        where poly' = fixPolygon2Chirality poly

-- | Try to find the boundaries sitting in a plane
-- Assumes slice is in XY plane
planeSlice :: [Face] -> Double -> Double -> Slice
planeSlice faces height z =
        let plane = Plane { planeNormal=(0,0,1), planePoint=bbMin .+^ (0,0,1) ^* z }
            proj (P (x,y,_)) = P (x,y)  -- | Project point onto XY plane
            projPolygon = map proj
            projLineSeg (LineSeg a b) = LineSeg (proj a) (proj b)  -- | Project line segment to XY plane
            projLineSegPath = map projLineSeg
            P (_,_,planeZ) = planePoint plane

            inPlane face = (abs (z - planeZ) < height) && (faceNormal face `parallel` (0,0,1))
                           where (P (_,_,z),_,_) = faceVertices face
            (inPlaneFaces, outPlaneFaces) = partition inPlane faces
            lines :: [LineSeg Vec3]
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
            fillPoly :: Polygon Vec2 -> Bool
            fillPoly poly = let path = polygonToLineSegPath poly
                                LineSeg a b = head path
                                ll = LineSeg origin $ alerp a b 0.5
                                inters = mapIntersection (lineSegLineSeg2Intersect ll)
                                       $ concat (deleteFirstsBy approx paths [path])
                            in length inters `mod` 2 == 1
        in (z, map (\poly->orientPolygon poly (fillPoly poly)) polys)

