{-# LANGUAGE TupleSections, PackageImports #-}

module Main(main) where

import Data.VectorSpace
import Data.AffineSpace
import Data.Maybe (mapMaybe, maybe)
import Data.Either (either, partitionEithers)
import Data.List (isSuffixOf, deleteFirstsBy)
import Text.Printf
import System.IO
import System.Environment (getArgs)
import "mtl" Control.Monad.State (evalState)

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

sliceZStep = 0.4

infillOffset = 0
infillSpacing = 1.05

infillPattern = hexInfill infillOffset infillSpacing

layerPostlude 1 _ = [ "F600" ]
layerPostlude _ _ = []

gcodeSettings = GCodeSettings
	{ gcPrelude = [ comment "Begin prelude"
                      , "F 1000"
	              , "G1 Z5"
                      , "G161 X0 Y0 Z0" 
	              , "G1 Z1"
                      , "G1 X80 Y50"
	              , "G1 Z0.30"
                      , "G92 X0 Y0 Z0"
                      , "F 300"
                      , comment "End prelude"
                      ]
	, gcLayerPrelude = (\_ _ -> [])
        , gcLayerPostlude = layerPostlude
	, gcPostlude = [ comment "Begin postlude"
		       , "G161 X0 Y0"
		       , "G161 Z0"
                       , "G1 Z5"
		       , "M104 S0"
		       , "M140 S0"
		       , comment "End postlude"
		       ]
	, gcRetractMinDist = 2 -- millimeter
	, gcRetractLength = 1 -- millimeter
	, gcRetractRate = 200 -- millimeter/min
	}

-- | When we slice exactly in the plane of a face, nasty things can happen with
-- numerical error. While we try to handle these properly, sliceFudge is one
-- final line of defence. We shift all of the slices by this amount to ensure
-- that we avoid these problems, even with nice even sliceZSteps.
sliceFudge = 1e-6

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
              toolpaths = zip sliceZs
	                $ evalState (mapM (toolPath infillPattern) slices) (igInitialState infillPattern)

          putStrLn "Rendering Slices..."
          mapM_ (doToolPath root region) toolpaths

          putStrLn "Generating GCode..."
          let gCode = slicesToGCode gcodeSettings toolpaths
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

