module GGen.ToolPath ( outlinePath
                     , infillPath
                     , toolPath
                     ) where

import Data.VectorSpace
import Data.AffineSpace
import Data.List (foldl', sortBy, deleteBy)
import Data.Function (on)

import GGen.Geometry.Types
import GGen.Geometry.Polygon (polygonToLineSegPath, linePolygon2Crossings)
import GGen.Geometry.Intersect (lineLine2Intersect)
import GGen.Geometry.BoundingBox (polygons2BoundingBox)
import GGen.Types

-- | Patch together a list of toolpaths into a single toolpath minimizing
-- unnecessary motion
concatToolPaths :: [ToolPath] -> ToolPath
concatToolPaths [] = []
concatToolPaths tps = f first (tail tps) (tpEnd first)
        where first = head tps
              tpDist p tp = magnitude (p .-. tpBegin tp)
              f :: ToolPath -> [ToolPath] -> Point2 -> ToolPath
              f tp [] _ = tp
              f tp tps pos = let nextToolPaths tp = [ (tpDist pos tp, tp, tps')
                                                    , (tpDist pos inverted, inverted, tps') ]
                                                  where inverted = tpInvert tp
                                                        tps' = deleteBy approx tp tps
                                 (_, next, tps') = head
                                                 $ sortBy (compare `on` (\(d,_,_)->d))
                                                 $ concat $ map nextToolPaths tps
                                 next' = ToolMove (LineSeg pos (tpBegin next)) Dry : next
                             in f (tp++next') tps' (tpEnd next)

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

-- | Figure out regions where infill is necessary
infillRegions :: [Polygon Vec2] -> [Polygon Vec2]
infillRegions = id

infillPattern :: InfillRatio -> Angle -> Box Vec2 -> [Line Vec2]
infillPattern infillRatio infillAngle (a,b) =
        map (\t -> Line (lBegin t) (cos phi, sin phi)) ts
        where ts = map (/40) [-40..40]
              phi = infillAngle / 180 * pi
              lBegin = alerp a (a .+^ magnitude (b.-.a) *^ (-sin phi, cos phi))

-- | Build the toolpath describing the infill of a slice
infillPath :: InfillRatio -> Angle -> [OrientedPolygon Vec2] -> ToolPath
infillPath infillRatio infillAngle opolys = 
        let polys = map fst opolys
            regions = infillRegions polys
            bb = polygons2BoundingBox polys
            pattern = infillPattern infillRatio infillAngle bb
            clipped = concat $ map (clipLine polys) pattern
        in concatToolPaths $ map (\l->extrudeLineSegPath [l]) clipped

-- | Build the toolpaths of a stack of slices
toolPath :: InfillRatio -> [Slice] -> [(Double, ToolPath)]
toolPath infillRatio slices = zipWith f slices (cycle [0, 60, 120])
        where f :: Slice -> Double -> (Double, ToolPath)
              f (z,opolys) infillAngle =
                      let outline = outlinePath opolys
                          infill = infillPath infillRatio infillAngle opolys
                      in (z, outline ++ infill)

-- QuickCheck properties

-- | Properties for clipLine

