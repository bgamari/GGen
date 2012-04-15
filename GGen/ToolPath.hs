{-# LANGUAGE PackageImports, TypeFamilies #-}

module GGen.ToolPath ( outlinePath
                     , toolPath
                     , infillPathM
                     -- * Infill patterns
                     , InfillPattern(..)
                     , linearInfill
                     , hexInfill
                     ) where

import Data.VectorSpace
import Data.AffineSpace
import Data.List (foldl', sortBy, deleteBy, partition)
import Data.Function (on)
import Control.Monad.Trans.State

import GGen.Geometry.Types
import GGen.Geometry.Clip
import GGen.Geometry.Polygon (polygonToLineSegPath, linePolygon2Crossings, offsetPolygon)
import GGen.Geometry.Intersect (lineLine2Intersect)
import GGen.Geometry.BoundingBox (polygons2BoundingBox)
import GGen.Types

-- | Patch together a list of toolpaths into a single toolpath minimizing
-- unnecessary motion
concatToolPaths :: [ToolPath] -> ToolPath
concatToolPaths tps 
        | null tps'  = []
        | otherwise = f first (tail tps') (tpEnd first)
        where tps' = filter (not.null) tps
              first = head tps'
              tpDist p tp = magnitude (p .-. tpBegin tp)
              f :: ToolPath -> [ToolPath] -> P2 -> ToolPath
              f tp [] _ = tp
              f tp rTps pos =
                      let nextToolPaths tp = [ (tpDist pos tp, tp, rTps')
                                             , (tpDist pos inverted, inverted, rTps') ]
                                           where inverted = tpInvert tp
                                                 rTps' = deleteBy approx tp rTps
                          (_, next, rTps') = head
                                           $ sortBy (compare `on` (\(d,_,_)->d))
                                           $ concat $ map nextToolPaths rTps
                          next' = ToolMove (LineSeg pos (tpBegin next)) Dry : next
                      in f (tp++next') rTps' (tpEnd next)

-- | Extrude path of line segments
extrudeLineSegPath :: LineSegPath R2 -> ToolPath
extrudeLineSegPath = map (\l -> ToolMove l (Extrude 1))

-- | Extrude path outlining polygon
extrudePolygon :: Polygon R2 -> ToolPath
extrudePolygon = extrudeLineSegPath . polygonToLineSegPath

-- | Build the toolpath describing the outline of a slice 
outlinePath :: [OrientedPolygon R2] -> ToolPath
outlinePath polys = concat $ map (extrudePolygon.fst) polys

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
                      , igPattern=pattern }
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

-- | Build the toolpaths of a stack of slices
toolPath :: InfillPattern s -> InfillPattern t -> Slice -> State (s,t) ToolPath
toolPath intPattern extPattern (_,opolys) = 
        do (intState, extState) <- get
           let (intPolys, extPolys) = partition (\(p,exposure) -> exposure==Internal) opolys
               (intInfill, intState') = runState (infillPathM intPattern (map fst intPolys)) intState
               (extInfill, extState') = runState (infillPathM extPattern (map fst extPolys)) extState
           put (intState', extState')
           return $ concatToolPaths [outlinePath (map fst opolys), intInfill, extInfill]

-- QuickCheck properties

-- | Properties for clipLine

