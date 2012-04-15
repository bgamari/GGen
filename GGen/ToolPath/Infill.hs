module GGen.ToolPath.Infill ( -- * General
                              InfillPattern
                            , infillPathM
                              -- * Infill patterns
                            , linearInfill
                            , hexInfill
                            ) where

import Data.VectorSpace
import Data.AffineSpace

import GGen.Types

-- | Generates an infill pattern
type PatternGen s = Box R2 -> State s [Line R2]
data InfillPattern s = InfillPattern { igInitialState :: s
                                     , igPattern :: PatternGen s
                                     } deriving (Show, Eq)

-- | Generate a set of lines filling the given box with the specified angle and
-- spacing
angledLinePattern :: Double -> Angle -> Double -> Box R2 -> [Line R2]
angledLinePattern spacing angle offset (a,b) =
        let l = magnitude (b .-. a)
            ts = map (offset+) [-l,-l+spacing..l]
            lBegin = alerp a (a .+^ r2 (-sin angle, cos angle))
        in map (\t -> Line (lBegin t) (r2 (cos angle, sin angle))) ts

-- | Simple infill of lines at a constant angle with constant spacing
linearInfill :: Double -> Angle -> InfillPattern ()
linearInfill spacing angle =
        InfillPattern { igInitialState=()
                      , igPattern=(\box -> return $ angledLinePattern spacing angle 0 box)
                      }

-- | Hexagonal infill
hexInfill :: Double -> Double -> InfillPattern ([Angle], [Double])
hexInfill = polyInfill (map (*(pi/180)) [0, 60, 120])

-- | General polygonal infill
polyInfill :: [Angle] -> Double -> Double -> InfillPattern ([Angle], [Double])
polyInfill angles offset infillSpacing =
        InfillPattern { igInitialState=(cycle angles, cycle [0,offset..infillSpacing])
                      , igPattern=pattern
                      }
        where pattern box =
                      do (angle:angles', offset:offsets') <- get
                         put (angles', offsets')
                         return $ angledLinePattern infillSpacing angle offset box

-- | Build the toolpath describing the infill of a slice
infillPathM :: InfillPattern s -> [OrientedPolygon R2] -> State s ToolPath
infillPathM pattern opolys = 
        do let polys = map fst opolys
               bb = polygons2BoundingBox polys
           pat <- (igPattern pattern) bb
           let clipped = concat $ map (clipLine polys) pat
           return $ concatToolPaths $ map (\l->extrudeLineSegPath [l]) clipped

