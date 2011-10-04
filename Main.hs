module Main(main) where

import Data.STL.Binary
import Data.STL.ASCII
import Data.VectorSpace
import Data.Maybe (mapMaybe)
import Data.Either (either)
import GGen.Geometry
import GGen.Geometry.Polygon
import qualified GGen.Pretty as P
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (($$), (<+>))

testMergeLineSegs = 
        do let a = LineSeg (0,0,0) (0.5,0,0)
               b = LineSeg (0.5,0,0) (1,0,0)
               c = LineSeg (0,0,0) (0,1,0)
           print $ PP.vcat $ map P.lineSeg $ mergeLineSegs' [a,b,c]

boundingBox :: STLFile -> (Vec, Vec)
boundingBox stl = let getAllVerts face = let (a,b,c) = faceVertices face
                                         in [a,b,c]
                      vs = concat $ map getAllVerts $ stlFacets stl
                      (xs,ys,zs) = unzip3 vs
                  in ( (minimum xs, minimum ys, minimum zs)
                     , (maximum xs, maximum ys, maximum zs) )

main = do stl <- Data.STL.Binary.parse "z-tensioner_1off.stl"
          let faces = stlFacets stl
          let plane = Plane { planeNormal=(0.0,0.0,1.0)
                            , planePoint=(0,0,10) }

          let (bbMin, bbMax) = boundingBox stl
          print $ PP.text "Bounding Box" <+> P.vec bbMin <+> PP.text "to" <+> P.vec bbMax
          --print $ PP.vcat $ map (\f->P.face f <+> PP.text "normal:" <+> (P.vec $ faceNormal f)) faces

          let boundaries = mapMaybe (planeFaceIntersect plane) faces
          print $ PP.vcat $ map P.lineSeg boundaries
          print ""
          print $ PP.vcat $ map (PP.braces . P.polygon) $ lineSegsToPolygons boundaries
          print ""

          let ps = planeSlice plane faces
          print $ PP.vcat $ map P.orientedPolygon ps
          
