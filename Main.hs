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
import GGen.ToolPath

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
              region = (bbMin - 0.2*^bbSize, bbMax + 0.2*^bbSize)

          print $ P.text "Bounding Box" <+> P.vec bbMin <+> P.text "to" <+> P.vec bbMax
          --print $ P.vcat $ map (\f->P.face f <+> P.text "normal:" <+> (P.vec $ faceNormal f)) faces
          
          let sliceZ = 1
              nSlices = (zMax-zMin) / sliceZ
              slices = map (\i->zMin + i*sliceZ) [0..nSlices]

          mapM_ (\z->doSlice faces root region z) slices

          return()

-- | stripSuffix a b strips the suffix a from list b
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix a b
        | null a            = Just b
        | a `isSuffixOf` b  = stripSuffix (init a) (init b)
        | otherwise         = Nothing

doSlice :: [Face] -> String -> Box Point -> Double -> IO ()
doSlice faces rootName region@(rMin,rMax) z = 
        do printf "Slice Z=%1.2f\n" z
           hFlush stdout

           let plane = Plane { planeNormal=(0,0,1)
                             , planePoint=rMin + (0,0,1) ^* z }
               opolys = planeSlice plane faces

           let outline = outlinePath opolys
               infill = infillPath 0.9 opolys

           let filename = printf "%s-z%1.2f.svg" rootName z
           --renderRegionToSVG filename (500,500) region (renderPolygons2 $ map fst ps)
           renderRegionToSVG filename (500,500) region $
                   do renderOrientedPolygons opolys
                      renderToolpath outline
                      renderToolpath infill

