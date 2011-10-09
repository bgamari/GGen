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
          
          --testSlice faces region ((zMax-zMin)/2)

          let sliceZ = 1
              nSlices = (zMax-zMin) / sliceZ
              slices = map (\i->zMin + i*sliceZ) [0..nSlices]
              sliceFilename = printf "%s-z%1.2f.svg" root :: Double -> String
          mapM_ (\z->renderSlice faces (sliceFilename z) region z) slices

          return()

proj (x,y,z) = (x,y)
projLineSeg (LineSeg a b) = LineSeg (proj a) (proj b)  -- | Project line segment to XY plane

testSlice :: [Face] -> Box Point -> Double -> IO ()
testSlice faces region z =
        do let (bbMin, bbMax) = region
               plane = Plane { planeNormal=(0,0,1)
                             , planePoint=bbMin + (0,0,1) ^* z }
               lines = mapIntersectionDropDegen (planeFaceIntersect plane) faces 
               paths = lineSegPaths $ mergeLineSegList $ map projLineSeg lines
               pps = mapMaybe (\path->do poly <- lineSegPathToPolygon path
                                         return (poly,path)
                              ) paths

           let origin = proj $ bbMax + (bbMax-bbMin) ^* 0.1
               render (r,g,b) path = 
                        do setSourceRGBA r g b 0.5
                           renderPath2 path
                           let LineSeg a b = head path
                               ll = LineSeg origin $ lerp a b 0.5
                               inters = mapIntersection (lineSegLineSeg2Intersect ll)
                                        $ concat (deleteFirstsBy approx paths [path])

                           newPath
                           drawSegment2 ll
                           stroke

                           mapM_ (\(x,y)->arc x y 1 0 (2*pi) >> fill) inters

           renderRegionToSVG "hi.svg" (500,500) region (do mapM_ ((render (0,1,0)).snd) pps
                                                           mapM_ ((render (1,0,0)).polygonToLineSegs.fst) pps)

           return ()


-- | stripSuffix a b strips the suffix a from list b
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix a b
        | null a            = Just b
        | a `isSuffixOf` b  = stripSuffix (init a) (init b)
        | otherwise         = Nothing

renderSlice :: [Face] -> FilePath -> Box Point -> Double -> IO ()
renderSlice faces filename region@(rMin,rMax) z = 
        do printf "Slice Z=%1.2f\r" z
           hFlush stdout
           let plane = Plane { planeNormal=(0,0,1)
                             , planePoint=rMin + (0,0,1) ^* z }
               ps = planeSlice plane faces

           --renderRegionToSVG filename (500,500) region (renderPolygons2 $ map fst ps)
           renderRegionToSVG filename (500,500) region (renderOrientedPolygons ps)

