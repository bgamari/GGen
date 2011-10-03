module Main(main) where

import GGen.Types
import GGen.Geometry
import Data.STL.Binary
import Numeric.LinearAlgebra
import Data.Maybe (mapMaybe, fromJust)
import qualified GGen.Pretty as P
import qualified Text.PrettyPrint.HughesPJ as PP

main = do stl <- parse "cube.stl"
          let faces = stlFacets stl
          --mapM_ (print.faceVertices) faces
          let plane = Plane { normal=fromList [0,0,1], point=fromList [0,0,0] }
          print $ PP.vcat $ map P.face faces
          print $ PP.vcat $ map P.lineSeg $ mapMaybe (planeFaceIntersect plane) faces
          print $ P.polygon $ fromJust $ planeSlice plane faces

