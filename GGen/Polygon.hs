module GGen.Polygon ( lineSegsToPolygon
                    , LSToPolyError(..)
                    , planeSlice
                    ) where

import Control.Monad (when)
import Control.Monad.Error
import Data.List (sortBy, delete, (\\))
import Data.Maybe (mapMaybe)
import Numeric.LinearAlgebra
import GGen.Types
import GGen.Geometry (invertLineSeg, mergeLineSegs', planeFaceIntersect)

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

-- | Try to match up a set of line segments into a closed polygon
lineSegsToPolygon :: [LineSeg] -> Either LSToPolyError Polygon
lineSegsToPolygon [] = Right []
lineSegsToPolygon segs@(LineSeg (a,_):_) = lineSegsToPolygon' [a] segs True

lineSegsToPolygon' :: Polygon -> [LineSeg] -> Bool -> Either LSToPolyError Polygon
lineSegsToPolygon' poly [] _
        | dist < pointTol  = Right poly
        | otherwise        = throwError $ CanNotClose poly
        where start = last poly
              end = head poly
              dist = norm2 (end-start)

lineSegsToPolygon' poly@(p:_) segs canFlip =
        do let dist (LineSeg (a,_)) = norm2 (p-a)
               targets = sortBy (\a b -> compare (dist a) (dist b))
                       $ filter (\l -> (dist l < pointTol)) segs
           if null targets
              then if canFlip
                      then lineSegsToPolygon' poly (map invertLineSeg segs) False
                      else throwError $ NoTargets poly segs
              else let LineSeg (a,b) = head targets
                   in lineSegsToPolygon' (b:poly) (delete (head targets) segs) True

-- | Try to find the boundaries sitting in a plane
planeSlice :: Plane -> [Face] -> Either LSToPolyError Polygon
planeSlice plane faces = let boundaries = mergeLineSegs' $ mapMaybe (planeFaceIntersect plane) faces
                         in lineSegsToPolygon boundaries
