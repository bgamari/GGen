module Main(main) where

import Data.VectorSpace
import Data.Maybe (mapMaybe, maybe)
import Data.Either (either)
import Data.List (isSuffixOf)
import Text.Printf
import System.IO
import System.Environment (getArgs)

import Data.STL
import GGen.Geometry
import GGen.Geometry.Polygon
import qualified GGen.Pretty as P
import GGen.Render
import Text.PrettyPrint.HughesPJ (($$), (<+>))

testMergeLineSegs = 
        do let a = LineSeg (0,0,0) (0.5,0,0)
               b = LineSeg (0.5,0,0) (1,0,0)
               c = LineSeg (0,0,0) (0,1,0)
           print $ P.vcat $ map P.lineSeg $ mergeLineSegs' [a,b,c]

boundingBox :: STLFile -> Box
boundingBox stl = let getAllVerts face = let (a,b,c) = faceVertices face
                                         in [a,b,c]
                      vs = concat $ map getAllVerts $ stlFacets stl
                      (xs,ys,zs) = unzip3 vs
                  in ( (minimum xs, minimum ys, minimum zs)
                     , (maximum xs, maximum ys, maximum zs) )

main = do filename:_ <- getArgs
          let root = maybe (error "Filename should end in .stl") id
                   $ stripSuffix ".stl" filename
          stl <- Data.STL.parse filename
          let faces = stlFacets stl
          let (bbMin, bbMax) = boundingBox stl
              bbSize = bbMax - bbMin
              (_,_,zMin) = bbMin
              (_,_,zMax) = bbMax
              region = (bbMin - 0.2*^bbSize, bbMax + 0.2*^bbSize)

          print $ P.text "Bounding Box" <+> P.vec bbMin <+> P.text "to" <+> P.vec bbMax
          --print $ P.vcat $ map (\f->P.face f <+> P.text "normal:" <+> (P.vec $ faceNormal f)) faces

          let plane = Plane { planeNormal=(0.0,0.0,1.0)
                            , planePoint=lerp bbMin bbMax 0.5 }
              ps = planeSlice plane faces
          --print $ P.vcat $ map P.orientedPolygon ps
          renderRegionToSVG "hi.svg" (500,500) region (renderPolygons $ map fst ps)

          let sliceZ = 1
              nSlices = (zMax-zMin) / sliceZ
              slices = map (\i->zMin + i*sliceZ) [0..nSlices]
              sliceFilename = printf "%s-z%1.2f.svg" root :: Double -> String
          mapM_ (\z->renderSlice faces (sliceFilename z) region z) slices

          return()

-- | stripSuffix a b strips the suffix a from list b
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix a b
        | null a            = Just b
        | a `isSuffixOf` b  = stripSuffix (init a) (init b)
        | otherwise         = Nothing

renderSlice :: [Face] -> FilePath -> Box -> Double -> IO ()
renderSlice faces filename (bbMin,bbMax) z = 
        do printf "Slice Z=%1.2f\r" z
           hFlush stdout
           let plane = Plane { planeNormal=(0,0,1)
                             , planePoint=bbMin + (0,0,1) ^* z }
               boundaries = mapMaybe (f . planeFaceIntersect plane) faces
               f l  | IIntersect l <- l = Just l
                    | INull <- l        = Nothing
                    | IDegenerate <- l  = Nothing -- TODO: Figure this out
           renderPathsToSVG filename (500,500) (bbMin,bbMax) $ lineSegPaths boundaries

          
