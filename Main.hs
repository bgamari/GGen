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
import GGen.Render

testMergeLineSegs = 
        do let a = LineSeg (0,0,0) (0.5,0,0)
               b = LineSeg (0.5,0,0) (1,0,0)
               c = LineSeg (0,0,0) (0,1,0)
           print $ PP.vcat $ map P.lineSeg $ mergeLineSegs' [a,b,c]

boundingBox :: STLFile -> Box
boundingBox stl = let getAllVerts face = let (a,b,c) = faceVertices face
                                         in [a,b,c]
                      vs = concat $ map getAllVerts $ stlFacets stl
                      (xs,ys,zs) = unzip3 vs
                  in ( (minimum xs, minimum ys, minimum zs)
                     , (maximum xs, maximum ys, maximum zs) )

main = do stl <- Data.STL.Binary.parse "z-tensioner_1off.stl"
          let faces = stlFacets stl
          let (bbMin, bbMax) = boundingBox stl
              bbSize = bbMax - bbMin
              (_,_,zMin) = bbMin
              (_,_,zMax) = bbMax
          let plane = Plane { planeNormal=(0.0,0.0,1.0)
                            , planePoint=lerp bbMin bbMax 0.5 }

          print $ PP.text "Bounding Box" <+> P.vec bbMin <+> PP.text "to" <+> P.vec bbMax
          --print $ PP.vcat $ map (\f->P.face f <+> PP.text "normal:" <+> (P.vec $ faceNormal f)) faces

          let boundaries = mapIntersection (planeFaceIntersect plane) faces

          --print $ PP.vcat $ map P.lineSeg boundaries
          --print ""
          --print $ PP.vcat $ map (PP.braces . P.polygon) $ lineSegsToPolygons boundaries
          --print ""

          --let ps = planeSlice plane faces
          --print $ PP.vcat $ map P.orientedPolygon ps

          let region = (bbMin - 0.2*^bbSize, bbMax + 0.2*^bbSize)
          mapM_ (\z->renderSlice faces ("slice-"++show z++".svg") region z) [1.1..25]

renderSlice :: [Face] -> FilePath -> Box -> Double -> IO ()
renderSlice faces filename (bbMin,bbMax) z = 
        do print z
           let plane = Plane { planeNormal=(0,0,1)
                             , planePoint=bbMin + (0,0,1) ^* z }
               boundaries = mapIntersection (planeFaceIntersect plane) faces
           renderPathsToSVG filename (500,500) (bbMin,bbMax) $ lineSegPaths boundaries

          
