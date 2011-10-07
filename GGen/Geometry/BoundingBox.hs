module GGen.Geometry.BoundingBox ( facesBoundingBox
                                 ) where

import GGen.Geometry.Types

facesBoundingBox :: [Face] -> Box
facesBoundingBox faces = let getAllVerts face = let (a,b,c) = faceVertices face
                                                in [a,b,c]
                             vs = concat $ map getAllVerts faces
                             (xs,ys,zs) = unzip3 vs
                  in ( (minimum xs, minimum ys, minimum zs)
                     , (maximum xs, maximum ys, maximum zs) )

