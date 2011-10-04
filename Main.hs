module Main(main) where

import Data.STL.Binary
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Utils (normalize)
import Data.Maybe (mapMaybe)
import Data.Either (either)
import GGen.Geometry
import GGen.Geometry.Polygon
import qualified GGen.Pretty as P
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (($$), (<+>))

testMergeLineSegs = 
        do let a = LineSeg (fromList [0,0,0]) (fromList [0.5,0,0])
               b = LineSeg (fromList [0.5,0,0]) (fromList [1,0,0])
               c = LineSeg (fromList [0,0,0]) (fromList [0,1,0])
           print $ PP.vcat $ map P.lineSeg $ mergeLineSegs' [a,b,c]

boundingBox :: STLFile -> (Vec, Vec)
boundingBox stl = let vecEnt n v = v @> n
                      getAllVerts face = let (a,b,c) = faceVertices face
                                         in [a,b,c]
                      vs = concat $ map getAllVerts $ stlFacets stl
                      xs = map (vecEnt 0) vs
                      ys = map (vecEnt 1) vs
                      zs = map (vecEnt 2) vs
                  in ( fromList [minimum xs, minimum ys, minimum zs]
                     , fromList [maximum xs, maximum ys, maximum zs] )

main = do stl <- parse "z-tensioner_1off.stl"
          let faces = stlFacets stl
          let plane = Plane { planeNormal=fromList [0.0,0.0,1.0]
                            , planePoint=fromList [0,0,10] }

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
          
