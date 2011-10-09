{-# LANGUAGE TupleSections #-}

module Main(main) where

import Data.VectorSpace
import Data.Maybe (mapMaybe, maybe)
import Data.Either (either, partitionEithers)
import Data.List (isSuffixOf, (\\))
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
          
          testSlice faces region ((zMax-zMin)/2)

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
           --renderRegionToSVG "hi.svg" (500,500) region (mapM_ (\l->drawSegment2 (projLineSeg l) >> stroke) $ mergeLineSegList lines)

           let origin = proj $ bbMax + (bbMax-bbMin) ^* 0.1
               orientPath :: LineSegPath Point2 -> IO (Either (LineSegPath Point2, LineSeg Point2) (OrientedPolygon Point2, LineSeg Point2, [Point2]))
               orientPath path = do let (LineSeg a b) = head path
                                        target = lerp a b 0.5
                                        ll = LineSeg origin target
                                        segments = concat (paths \\ [path])
                                        intersects = mapIntersectionDropDegen (lineSegLineSeg2Intersect ll) segments
                                        fill = length intersects `mod` 2 == 1
                                    return $ case lineSegPathToPolygon path of
                                                  Just poly -> Right ((poly, fill), ll, intersects)
                                                  Nothing   -> Left (path, ll)

           (fails, polys) <- liftM partitionEithers $ mapM orientPath paths

           let render = do renderOrientedPolygons $ map (\(a,b,c)->a) polys
                           mapM_ (\(poly,ll,inters) -> do setSourceRGBA 0 0 1 0.5
                                                          newPath
                                                          drawSegment2 ll
                                                          stroke

                                                          setSourceRGBA 0 1 0 0.5
                                                          mapM_ (\i->arc (fst i) (snd i) 1 0 (2*pi) >> fill) inters
                                 ) polys

                           setSourceRGBA 1 1 0 0.5
                           mapM_ (\(path,ll) -> renderPath2 path) fails
           renderRegionToSVG "hi.svg" (500,500) region render

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

