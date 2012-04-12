{-# LANGUAGE TupleSections, PackageImports #-}

module GGen( GGenSettings(..)
           , slice
           , ggenMain
           -- For convenience
           , hexInfill
           , comment
           , GCodeSettings(..)
           ) where

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
import GGen.Slice
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

data GGenSettings s = GGenSettings { ggSliceZStep :: Double
                                   , ggInfillPattern :: InfillPattern s
                                   , ggGCodeSettings :: GCodeSettings
                                   } deriving (Show, Eq)

-- | When we slice exactly in the plane of a face, nasty things can happen with
-- numerical error. While we try to handle these properly, sliceFudge is one
-- final line of defence. We shift all of the slices by this amount to ensure
-- that we avoid these problems, even with nice even sliceZSteps.
sliceFudge = 1e-6

ggenMain :: GGenSettings s -> IO ()
ggenMain settings =
        do filename:_ <- getArgs
           let root = maybe (error "Filename should end in .stl") id
                    $ stripSuffix ".stl" filename
           stl <- Data.STL.parse filename
           slices <- slice settings (stlFacets stl)

           let (bbMin, bbMax) = facesBoundingBox (stlFacets stl)
               bbSize = bbMax .-. bbMin
               region = (bbMin .-^ 0.2*^bbSize, bbMax .+^ 0.2*^bbSize)
           mapM_ (renderSlice root region) slices

           let gcode = slicesToGCode (ggGCodeSettings settings) slices
           writeFile (root++".gcode") (unlines gcode)
           return ()

slice :: GGenSettings s -> [Face] -> IO [(Double, ToolPath)]
slice settings faces =
        do let zStep = ggSliceZStep settings
               infill = ggInfillPattern settings
               extInfill = linearInfill 0.4 0
           let (bbMin, bbMax) = facesBoundingBox faces
               bbSize = bbMax .-. bbMin
               (_,_,zMin) = unp3 bbMin
               (_,_,zMax) = unp3 bbMax
               region@(rMin,rMax) = (bbMin .-^ 0.2*^bbSize, bbMax .+^ 0.2*^bbSize)

           let nSlices = (zMax-zMin) / zStep
               sliceZs = map (\i->zMin + i*zStep + sliceFudge) [0..nSlices]
               slices = map (planeSlice faces zStep) sliceZs
               toolpaths = zip sliceZs
                         $ evalState (mapM (toolPath infill extInfill) slices) (igInitialState infill, igInitialState extInfill)

           return toolpaths

-- | stripSuffix a b strips the suffix a from list b
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix a b
        | null a            = Just b
        | a `isSuffixOf` b  = stripSuffix (init a) (init b)
        | otherwise         = Nothing

renderSlice :: String -> Box R3 -> (Double, ToolPath) -> IO ()
renderSlice rootName region@(rMin,rMax) (z,tp) =
        do printf "Slice Z=%1.2f\n" z
           renderRegionToSVG filename (500,500) region $
                   do renderToolpath tp
        where filename = printf "%s-z%1.2f.svg" rootName z

