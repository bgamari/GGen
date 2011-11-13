{-# LANGUAGE PackageImports, TypeFamilies #-}

module GGen.ToolPath ( outlinePath
                     , infillPathM
                     , toolPath
                     , hexInfill
                     , InfillPattern(..)
                     ) where

import Data.VectorSpace
import Data.AffineSpace
import Data.List (foldl', sortBy, deleteBy)
import Data.Function (on)
import "mtl" Control.Monad.State

import GGen.Geometry.Types
import GGen.Geometry.Polygon (polygonToLineSegPath, linePolygon2Crossings)
import GGen.Geometry.Intersect (lineLine2Intersect)
import GGen.Geometry.BoundingBox (polygons2BoundingBox)
import GGen.Types

import Debug.Trace

-- | Patch together a list of toolpaths into a single toolpath minimizing
-- unnecessary motion
concatToolPaths :: [ToolPath] -> ToolPath
concatToolPaths tps 
        | null tps'  = []
        | otherwise = f first (tail tps') (tpEnd first)
        where tps' = filter (not.null) tps
              first = head tps'
              tpDist p tp = magnitude (p .-. tpBegin tp)
              f :: ToolPath -> [ToolPath] -> Point2 -> ToolPath
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
extrudeLineSegPath :: LineSegPath Vec2 -> ToolPath
extrudeLineSegPath = map (\l -> ToolMove l (Extrude 1))

-- | Extrude path outlining polygon
extrudePolygon :: Polygon Vec2 -> ToolPath
extrudePolygon = extrudeLineSegPath . polygonToLineSegPath

-- | Offset polygon boundaries inward or outwards
-- Positive offset is outwards
offsetPolygon :: Double -> Polygon Vec2 -> Polygon Vec2
offsetPolygon offset = Polygon . f . polygonToLineSegPath
        where f :: LineSegPath Vec2 -> [Point2]
              f segs@(s:s':_) = 
                let p = lsB s
                    l  = Line (p .+^ ls2Normal s RightHanded)  (offset *^ normalized (lsDispl s))
                    l' = Line (p .+^ ls2Normal s' RightHanded) (offset *^ normalized (lsDispl s'))
                    IIntersect p' = lineLine2Intersect l l'
                in p':f (tail segs)

-- | Build the toolpath describing the outline of a slice 
outlinePath :: [OrientedPolygon Vec2] -> ToolPath
outlinePath polys = concat $ map (extrudePolygon.fst) polys

-- | Clip a line with a set of polygons
clipLine :: [Polygon Vec2] -> Line Vec2 -> [LineSeg Vec2]
clipLine polys line = 
        let inters = concat $ map (linePolygon2Crossings line) polys
            cmpInter (P (ax,ay)) (P (bx,by)) = case compare ax bx of 
                                                    EQ -> compare ay by
                                                    c  -> c
            sorted = sortBy cmpInter inters

            f :: [Point2] -> Bool -> [LineSeg Vec2]
            f points@(a:b:_) fill = if fill then (LineSeg a b) : (f (tail points) False)
                                            else f (tail points) True
            f (a:[]) True = error $ "Unterminated line segment "++show sorted++" while clipping "++show line++" against polygon "++show polys
            f _ _ = []
        in f sorted True

-- | Generates an infill pattern
type PatternGen s = Box Vec2 -> State s [Line Vec2]
data InfillPattern s = InfillPattern { igInitialState :: s
                                     , igPattern :: PatternGen s
                                     } deriving (Show, Eq)

hexInfill :: Double -> Double -> InfillPattern ([Angle], [Double])
hexInfill = polyInfill (map (*(pi/180)) [0, 60, 120])

polyInfill :: [Angle] -> Double -> Double -> InfillPattern ([Angle], [Double])
polyInfill angles offset infillSpacing =
        InfillPattern { igInitialState=(cycle angles, cycle [0,offset..infillSpacing])
                      , igPattern=pattern }
        where pattern (a,b) =
                      do (phi:angles', offset:offsets') <- get
                         put (angles', offsets')
                         let ts = map (offset+) [0,infillSpacing..magnitude (b.-.a)]
                             lBegin = alerp a (a .+^ (-sin phi, cos phi))
                         return $ tr $ map (\t -> Line (lBegin t) (cos phi, sin phi)) ts

tr x = traceShow x x

-- | Build the toolpath describing the infill of a slice
infillPathM :: InfillPattern s -> [OrientedPolygon Vec2] -> State s ToolPath
infillPathM pattern opolys = 
        do let polys = map fst opolys
               bb = polygons2BoundingBox polys
           pat <- (igPattern pattern) bb
           let clipped = concat $ map (clipLine polys) pat
           return $ concatToolPaths $ map (\l->extrudeLineSegPath [l]) clipped

-- | Build the toolpaths of a stack of slices
toolPath :: InfillPattern s -> Slice -> State s ToolPath
toolPath pattern (_,opolys) = 
        do infill <- infillPathM pattern opolys
           return $ concatToolPaths [outlinePath opolys, infill]

-- QuickCheck properties

-- | Properties for clipLine

