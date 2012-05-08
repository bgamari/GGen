{-# LANGUAGE TupleSections, PackageImports #-}

module GGen( GGenSettings(..)
           , slice
           , ggenMain
           -- For convenience
           , hexInfill
           , comment, command
           ) where

import           Control.Monad.Trans.State (evalState)
import           Data.AffineSpace
import           Data.Either (either, partitionEithers)
import           Data.List (isSuffixOf, deleteFirstsBy)
import           Data.Maybe (mapMaybe, maybe)
import qualified Data.Text.Lazy.IO as TIO
import           Data.VectorSpace
import           System.Environment (getArgs)
import           System.IO
import           Text.Printf

import           Data.STL
import           GGen.Slice
import           GGen.Geometry
import           GGen.Geometry.Polygon
import qualified GGen.Pretty as P
import           GGen.Render
import           Text.PrettyPrint.HughesPJ (($$), (<+>))
import           GGen.Types
import           GGen.ToolPath
import           GGen.ToolPath.Infill
import           GGen.ToolPath.Extruder
import           GGen.GCode
import           GGen.GCode.Extruder

import           Graphics.Rendering.Cairo
import           Control.Monad (liftM)

data GGenSettings tool infill
     = GGenSettings { ggSliceZStep :: Double
                    , ggInfillPattern :: InfillPattern infill
                    , ggToolSettings :: tool
                    }

-- | When we slice exactly in the plane of a face, nasty things can happen with
-- numerical error. While we try to handle these properly, sliceFudge is one
-- final line of defence. We shift all of the slices by this amount to ensure
-- that we avoid these problems, even with nice even sliceZSteps.
sliceFudge = 1e-6

ggenMain :: GGenSettings ExtruderSettings infill -> IO ()
ggenMain settings =
        do filename:_ <- getArgs
           let root = maybe (error "Filename should end in .stl") id
                    $ stripSuffix ".stl" filename
           stl <- Data.STL.parse filename
           let slices = slice settings $ stlFacets stl

           let (bbMin, bbMax) = facesBoundingBox (stlFacets stl)
               bbSize = bbMax .-. bbMin
               region = (bbMin .-^ 0.2*^bbSize, bbMax .+^ 0.2*^bbSize)
           mapM_ (renderSlice root region) slices

           let gcode = slicesToGCode (ggToolSettings settings) slices
           TIO.writeFile (root++".gcode") gcode
           return ()

slice :: GGenSettings ExtruderSettings infill -> [Face] -> [(Double, ToolPath marker ExtruderMove)]
slice settings faces =
        let zStep = ggSliceZStep settings
            infill = ggInfillPattern settings
            extInfill = linearInfill 0.4 0
            (bbMin, bbMax) = facesBoundingBox faces
            bbSize = bbMax .-. bbMin
            (_,_,zMin) = unp3 bbMin
            (_,_,zMax) = unp3 bbMax
            region@(rMin,rMax) = (bbMin .-^ 0.2*^bbSize, bbMax .+^ 0.2*^bbSize)

            nSlices = (zMax-zMin) / zStep
            sliceZs = map (\i->zMin + i*zStep + sliceFudge) [0..nSlices]
            slices = map (planeSlice faces zStep) sliceZs
        in zip sliceZs
           $ evalState (mapM (toolPath (Extrude 1) infill extInfill) slices)
                       (igInitialState infill, igInitialState extInfill)

-- | stripSuffix a b strips the suffix a from list b
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix a b
        | null a            = Just b
        | a `isSuffixOf` b  = stripSuffix (init a) (init b)
        | otherwise         = Nothing

renderSlice :: String -> Box R3 -> (Double, ToolPath marker tool) -> IO ()
renderSlice rootName region@(rMin,rMax) (z,tp) =
        do printf "Slice Z=%1.2f\n" z
           renderRegionToSVG filename (500,500) region $
                   do renderToolpath tp
        where filename = printf "%s-z%1.2f.svg" rootName z

