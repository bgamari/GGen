module Main(main) where

import Data.STL.Binary
import Numeric.LinearAlgebra
import Data.Maybe (mapMaybe)
import Data.Either (either)
import GGen.Types
import GGen.Geometry
import GGen.Polygon
import qualified GGen.Pretty as P
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (($$), (<+>))

normalize v = 1 / norm2 v `scale` v

testMergeLineSegs = 
        do let a = LineSeg (fromList [0,0,0], fromList [0.5,0,0])
               b = LineSeg (fromList [0.5,0,0], fromList [1,0,0])
               c = LineSeg (fromList [0,0,0], fromList [0,1,0])
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

main = do stl <- parse "cube-hole.stl"
          let faces = stlFacets stl
          let plane = Plane { planeNormal=fromList [0.0,0.0,1.0]
                            , planePoint=fromList [0,0,0] }
          let (bbMin, bbMax) = boundingBox stl
          print $ PP.text "Bounding Box" <+> P.vec bbMin <+> PP.text "to" <+> P.vec bbMax
          --print $ PP.vcat $ map (\f->P.face f <+> PP.text "normal:" <+> (P.vec $ faceNormal f)) faces

          let boundaries = mapMaybe (planeFaceIntersect plane) faces
              ps = either (error . show . P.lsToPolyError) id $ planeSlice plane faces
          print $ PP.vcat $ map P.polygon ps
          
