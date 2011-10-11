{-# LANGUAGE TupleSections #-}

module Main(main) where

import Data.VectorSpace
import Data.Maybe (mapMaybe, maybe)
import Data.Either (either, partitionEithers)
import Data.List (isSuffixOf, deleteFirstsBy)
import Text.Printf
import System.IO
import System.Environment (getArgs)

import Data.STL
import GGen.Geometry
import GGen.Geometry.Polygon
import qualified GGen.Pretty as P
import GGen.Render
import Text.PrettyPrint.HughesPJ (($$), (<+>))
import GGen.Types
import GGen.ToolPath
import GGen.GCode

import Graphics.Rendering.Cairo
import Control.Monad (liftM)

main = do filename:_ <- getArgs
          let root = maybe (error "Filename should end in .stl") id
                   $ stripSuffix ".stl" filename
          stl <- Data.STL.parse filename
          let faces = stlFacets stl
          let (bbMin, bbMax) = facesBoundingBox $ stlFacets stl
              bbSize = bbMax - bbMin
              (_,_,zMin) = bbMin
              (_,_,zMax) = bbMax
              region@(rMin,rMax) = (bbMin - 0.2*^bbSize, bbMax + 0.2*^bbSize)

          let --p = Polygon [(7.013573169708252,0.0),(0.0,0.0),(0.0,9.990506172180176),(6.801092164143565,9.990506172180176),(7.602372827779513,9.990506172180176),(22.335197380293096,9.990506172180176),(28.0,9.990506172180176),(28.0,0.0),(21.013572692871094,0.0),(21.013572692871094,2.992628574371338),(18.52680539057239,2.992628574371338),(17.816872779664656,2.992628574371338),(8.919278841077409,2.992628574371338),(7.013573169708252,2.992628574371338)]
              p = Polygon [(7.013573169708252,0.0),(0.0,0.0),(0.0,9.990506172180176),(6.801092164143565,9.990506172180176),(7.602372827779513,9.990506172180176),(22.335197380293096,9.990506172180176)]
              l = Line {lPoint = (0.0,0.0), lDir = (0.8660254037844386,0.5000000000000001)}
          --print $ linePolygon2Crossings l p

          print $ P.text "Bounding Box" <+> P.vec bbMin <+> P.text "to" <+> P.vec bbMax
          --print $ P.vcat $ map (\f->P.face f <+> P.text "normal:" <+> (P.vec $ faceNormal f)) faces
          
          let getSlice z = let plane = Plane { planeNormal=(0,0,1)
                                             , planePoint=rMin + (0,0,1) ^* z }
                               opolys = planeSlice plane faces
                           in (z, opolys)

          let sliceZStep = 1
              nSlices = (zMax-zMin) / sliceZStep
              sliceZs = map (\i->zMin + i*sliceZStep) [0..nSlices]
              slices = map getSlice sliceZs
              toolpaths = toolPath 0.5 slices

          --mapM_ (\z->doSlice faces root region z) sliceZs
          mapM_ (doToolPath root region) toolpaths
          let gCode = slicesToGCode toolpaths
          writeFile (root++".gcode") $ unlines gCode

          return()

-- | stripSuffix a b strips the suffix a from list b
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix a b
        | null a            = Just b
        | a `isSuffixOf` b  = stripSuffix (init a) (init b)
        | otherwise         = Nothing

doToolPath :: String -> Box Point -> (Double, ToolPath) -> IO ()
doToolPath rootName region@(rMin,rMax) (z,tp) =
        do printf "Slice Z=%1.2f\n" z
           renderRegionToSVG filename (500,500) region $
                   do renderToolpath tp
        where filename = printf "%s-z%1.2f.svg" rootName z

doSlice :: [Face] -> String -> Box Point -> Double -> IO ()
doSlice faces rootName region@(rMin,rMax) z = 
        do printf "Slice Z=%1.2f\n" z
           hFlush stdout

           let plane = Plane { planeNormal=(0,0,1)
                             , planePoint=rMin + (0,0,1) ^* z }
               opolys = planeSlice plane faces
               filename = printf "%s-z%1.2f-slice.svg" rootName z

           let outline = outlinePath opolys
               infill = infillPath 0.5 45 opolys

           --renderRegionToSVG filename (500,500) region (renderPolygons2 $ map fst ps)
           renderRegionToSVG filename (500,500) region $
                   do renderOrientedPolygons opolys
                      renderToolpath outline
                      renderToolpath infill

                      setSourceRGB 1 1 0
                      moveTo 0 0
                      lineTo 17.3 9.99
                      stroke

