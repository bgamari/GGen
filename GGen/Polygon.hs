module GGen.Polygon ( lineSegsToPolygons
                    , polygonToLineSegs
                    , LSToPolyError(..)
                    , planeSlice
                    , rayPolygonIntersects
                    , OrientedPolygon
                    , lineSegPaths
                    ) where

import Debug.Trace
import Text.PrettyPrint.HughesPJ as PP

import Control.Monad (when)
import Control.Monad.Error
import Data.List (sortBy, delete, (\\), foldl')
import Data.Maybe (fromJust, mapMaybe, catMaybes, listToMaybe)
import Numeric.LinearAlgebra
import GGen.Types
import GGen.Geometry

approx :: Int -> Double -> Double
approx places x = (realToFrac $ round $ 10^places*x) / 10^places
approxDouble = PP.double . approx 2
vec v = parens $ hcat $ punctuate (text ", ") $ map approxDouble $ toList v



-- | The maximum distance between identical points
pointTol = 1e-4

-- | The maximum deviation from one in a dot product to consider vectors parallel
dirTol = 1e-4

-- | Describes an error while converting line segments to a polygon
data LSToPolyError = CanNotClose Polygon
                   | NoTargets Polygon [LineSeg]
                   | OtherError String
                   deriving (Show)

instance Error LSToPolyError where
        strMsg = OtherError

lineSegBegin, lineSegEnd :: LineSeg -> Point
lineSegBegin (LineSeg (a,_)) = a
lineSegEnd (LineSeg (_,a)) = a

type LineSegPath = [LineSeg]

-- | Find contiguous paths of line segments
lineSegPaths :: [LineSeg] -> [LineSegPath]
lineSegPaths =
        -- | Find the path to which a given segment should be added
        let findTarget' :: [LineSegPath] -> LineSeg -> Maybe LineSegPath
            findTarget' paths l = let dist path = norm2 $ (lineSegEnd l) - (lineSegBegin $ head path)
                                  in    listToMaybe 
                                      $ sortBy (\p p' -> compare (dist p) (dist p'))
                                      $ filter (\p -> dist p < pointTol) paths
            findTarget paths l = head $ catMaybes [ findTarget' paths l
                                                  , findTarget' paths (invertLineSeg l)
                                                  , findTarget' (map reverse paths) l -- TODO: Oh the horror
                                                  , findTarget' (map reverse paths) (invertLineSeg l)
                                                  , Just [] ]
            addSeg paths l = let target = findTarget paths l
                                 target' = l:target
                             in target' : (paths \\ [target])
        in foldl' addSeg []

-- | Try to match up a set of line segments into a closed polygon
lineSegsToPolygons :: [LineSeg] -> Either LSToPolyError [Polygon]
lineSegsToPolygons [] = Right []
lineSegsToPolygons segs = 
        let f [] = Right []
            f segs@(LineSeg (a,_):_) = do (poly, segs') <- lineSegsToPolygon' [a] segs True
                                          rest <- f segs'
                                          return $ poly:rest
        in f segs

lineSegsToPolygon' :: Polygon -> [LineSeg] -> Bool -> Either LSToPolyError (Polygon, [LineSeg])
lineSegsToPolygon' poly [] _
        | dist < pointTol  = Right (poly, [])
        | otherwise        = throwError $ CanNotClose poly
        where start = last poly
              end = head poly
              dist = norm2 (end-start)

lineSegsToPolygon' poly@(p:_) segs canFlip =
        do let dist (LineSeg (a,_)) = norm2 (p-a)
               targets = sortBy (\a b -> compare (dist a) (dist b))
                       $ filter (\l -> (dist l < pointTol)) segs
           if null targets
              then if samePoint (head poly) (last poly)
                      then Right (poly, segs)
                      else if canFlip
                              then lineSegsToPolygon' poly (map invertLineSeg segs) False
                              else throwError $ NoTargets poly segs
              else let LineSeg (a,b) = head targets
                   in lineSegsToPolygon' (b:poly) (delete (head targets) segs) True

-- | (poly, True) refers to a polygon poly which should have its interior filled
type OrientedPolygon = (Polygon, Bool)

-- | Points of intersection between a ray and a polygon
rayPolygonIntersects :: Ray -> Polygon -> [Point]
rayPolygonIntersects ray poly =
        mapMaybe (rayLineSegIntersect ray) $ polygonToLineSegs poly

-- | Get line segments of polygon boundary
polygonToLineSegs :: Polygon -> [LineSeg]
polygonToLineSegs (_:[]) = []
polygonToLineSegs poly@(a:b:_) = (LineSeg (a,b)) : (polygonToLineSegs $ tail poly)

-- | Try to find the boundaries sitting in a plane
-- In order to identify the interior of each polygon, we build a map from line
-- segment endpoints to their corresponding faces so we can later find the normals. 
planeSlice :: Plane -> [Face] -> Either LSToPolyError [OrientedPolygon]
planeSlice plane faces =
        do let f face = maybe Nothing (\ls -> Just (ls,face)) $ planeFaceIntersect plane face
               boundaries = catMaybes $ map f faces
               mergedLines = mergeLineSegs' $ map fst boundaries

               -- Map from endpoints to faces
               pointMap = concat $ map (\(LineSeg (a,b), face) -> [(a,face), (b,face)]) boundaries

               -- Figure out whether polygon should be filled
               orientPolygon :: Polygon -> OrientedPolygon
               orientPolygon poly = let origin = head poly
                                        face = fromJust $ lookup origin pointMap
                                        n = planeFaceNormal plane face
                                        intersects = length $ rayPolygonIntersects (Ray (origin,n)) poly
                                        hi = text "Intersects" <+> (hsep $ map vec $ rayPolygonIntersects (Ray (origin,n)) poly)
                                          $$ text "Origin" <+> vec origin
                                          $$ text "Normal" <+> vec n
                                          $$ text ""
                                        fill = trace (show hi) $ intersects `mod` 2 == 1
                                    in (poly, fill)
           polygons <- lineSegsToPolygons mergedLines
           return $ map orientPolygon polygons


