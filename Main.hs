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
import Text.PrettyPrint.HughesPJ (($$))


testMergeLineSegs = 
        do let a = LineSeg (fromList [0,0,0], fromList [0.5,0,0])
               b = LineSeg (fromList [0.5,0,0], fromList [1,0,0])
               c = LineSeg (fromList [0,0,0], fromList [0,1,0])
           print $ PP.vcat $ map P.lineSeg $ mergeLineSegs' [a,b,c]

main = do stl <- parse "cube.stl"
          let faces = stlFacets stl
          let plane = Plane { normal=fromList [0,0,1], point=fromList [0,0,0] }
          --print $ PP.vcat $ map P.face faces

          let boundaries = mapMaybe (planeFaceIntersect plane) faces
          print $ PP.vcat $ map P.lineSeg $ mergeLineSegs' $ boundaries
          print $ P.polygon $ either (error.show.P.lsToPolyError) id $ planeSlice plane faces

