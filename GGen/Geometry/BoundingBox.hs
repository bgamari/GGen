module GGen.Geometry.BoundingBox ( points2BoundingBox
                                 , pointsBoundingBox
                                 , facesBoundingBox
                                 , polygons2BoundingBox
                                 ) where

import GGen.Geometry.Types

points2BoundingBox :: [Point2] -> Box Point2
points2BoundingBox points =
        let (xs,ys) = unzip points
        in ( (minimum xs, minimum ys)
           , (maximum xs, maximum ys) )

pointsBoundingBox :: [Point] -> Box Point
pointsBoundingBox points =
        let (xs,ys,zs) = unzip3 points
        in ( (minimum xs, minimum ys, minimum zs)
           , (maximum xs, maximum ys, maximum zs) )

facesBoundingBox :: [Face] -> Box Point
facesBoundingBox faces =
        let getAllVerts face = let (a,b,c) = faceVertices face
                               in [a,b,c]
        in pointsBoundingBox $ concat $ map getAllVerts faces

polygons2BoundingBox :: [Polygon Point2] -> Box Point2
polygons2BoundingBox polys = points2BoundingBox $ concat $ map (\(Polygon points) -> points) polys

