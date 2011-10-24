{-# LANGUAGE TupleSections #-}

module Main(main) where

import Data.VectorSpace
import Data.AffineSpace
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

-- | When we slice directly in the plane of a face, nasty numerical things can
-- happen. While we try to handle these properly, sliceFudge is one final line
-- of defence. We shift all of the slices by this amount to ensure that we
-- avoid these problems, even with nice even sliceZSteps.
sliceFudge = 1e-4

sliceZStep = 0.2

main = do filename:_ <- getArgs
          let root = maybe (error "Filename should end in .stl") id
                   $ stripSuffix ".stl" filename
          stl <- Data.STL.parse filename
          let faces = stlFacets stl
          let (bbMin, bbMax) = facesBoundingBox $ stlFacets stl
              bbSize = bbMax .-. bbMin
              P (_,_,zMin) = bbMin
              P (_,_,zMax) = bbMax
              region@(rMin,rMax) = (bbMin .-^ 0.2*^bbSize, bbMax .+^ 0.2*^bbSize)

          print $ P.text "Bounding Box" <+> P.point bbMin <+> P.text "to" <+> P.point bbMax
          --print $ P.vcat $ map (\f->P.face f <+> P.text "normal:" <+> (P.vec $ faceNormal f)) faces
          
          let getSlice z = let plane = Plane { planeNormal=(0,0,1)
                                             , planePoint=bbMin .+^ (0,0,1) ^* z }
                               opolys = planeSlice plane faces
                           in (z, opolys)

          let nSlices = (zMax-zMin) / sliceZStep
              sliceZs = map (\i->zMin + i*sliceZStep + sliceFudge) [0..nSlices]
              slices = map getSlice sliceZs
              toolpaths = toolPath 0.5 slices

          putStrLn "Rendering Slices..."
          --mapM_ (\z->doSlice faces root (bbMin,bbMax) z) sliceZs
          mapM_ (doToolPath root region) toolpaths

          putStrLn "Generating GCode..."
          let gCode = slicesToGCode toolpaths
          writeFile (root++".gcode") $ unlines gCode

          return()

-- | stripSuffix a b strips the suffix a from list b
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix a b
        | null a            = Just b
        | a `isSuffixOf` b  = stripSuffix (init a) (init b)
        | otherwise         = Nothing

doToolPath :: String -> Box Vec3 -> (Double, ToolPath) -> IO ()
doToolPath rootName region@(rMin,rMax) (z,tp) =
        do printf "Slice Z=%1.2f\n" z
           renderRegionToSVG filename (500,500) region $
                   do renderToolpath tp
        where filename = printf "%s-z%1.2f.svg" rootName z

doSlice :: [Face] -> String -> Box Vec3 -> Double -> IO ()
doSlice faces rootName (bbMin,bbMax) z = 
        do printf "Slice Z=%1.2f\n" z
           hFlush stdout

           let plane = Plane { planeNormal=(0,0,1)
                             , planePoint=bbMin .+^ (0,0,1) ^* z }
               opolys = planeSlice plane faces
               filename = printf "%s-z%1.2f-slice.svg" rootName z

           let outline = outlinePath opolys
               infill = infillPath 0.5 45 opolys

           --renderRegionToSVG filename (500,500) region (renderPolygons2 $ map fst ps)
           renderRegionToSVG filename (500,500) region $
                   do renderOrientedPolygons opolys
                      --renderToolpath outline
                      --renderToolpath infill
         where bbSize = bbMax .-. bbMin
               region = (bbMin .-^ 0.2*^bbSize, bbMax .+^ 0.2*^bbSize)
