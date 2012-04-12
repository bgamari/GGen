module GGen.Geometry.BoundingBox ( points2BoundingBox
                                 , points3BoundingBox
                                 , facesBoundingBox
                                 , polygons2BoundingBox
                                 ) where

import Data.VectorSpace
import GGen.Geometry.Types

points2BoundingBox :: [P2] -> Box R2
points2BoundingBox [] = (p2 (0,0), p2 (0,0))
points2BoundingBox points =
        let (xs,ys) = unzip $ map unp2 points
        in ( p2 (minimum xs, minimum ys)
           , p2 (maximum xs, maximum ys) )

points3BoundingBox :: [P3] -> Box R3
points3BoundingBox points =
        let (xs,ys,zs) = unzip3 $ map unp3 points
        in ( p3 (minimum xs, minimum ys, minimum zs)
           , p3 (maximum xs, maximum ys, maximum zs) )

facesBoundingBox :: [Face] -> Box R3
facesBoundingBox faces =
        let getAllVerts face = let (a,b,c) = faceVertices face
                               in [a,b,c]
        in points3BoundingBox $ concat $ map getAllVerts faces

polygons2BoundingBox :: [Polygon R2] -> Box R2
polygons2BoundingBox polys = points2BoundingBox $ concat $ map (\(Polygon points) -> points) polys

