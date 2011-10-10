module GGen.ToolPath ( outlinePath
                     , infillPath
                     , clipLine
                     ) where

import Data.VectorSpace
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
              tpDist p tp = magnitude (p - tpBegin tp)
              f :: ToolPath -> [ToolPath] -> Point2 -> ToolPath
              f tp [] _ = tp
              f tp tps pos = let next = snd $ head -- TODO: Reverse polygons
                                      $ sortBy (compare `on` fst)
                                      $ map (\tp->(tpDist pos tp, tp)) tps
                             in f (tp++next) (deleteBy approx next tps) (tpEnd next)

-- | Extrude path of line segments
extrudeLineSegPath :: LineSegPath Point2 -> ToolPath
extrudeLineSegPath = map (\l -> ToolMove l (Extrude 1))

-- | Extrude path outlining polygon
extrudePolygon :: Polygon Point2 -> ToolPath
extrudePolygon = extrudeLineSegPath . polygonToLineSegPath

-- | Offset polygon boundaries inward or outwards
-- Positive offset is outwards
offsetPolygon :: Double -> Polygon Point2 -> Polygon Point2
offsetPolygon offset = Polygon . f . polygonToLineSegPath
        where f :: LineSegPath Point2 -> [Point2]
              f segs@(s:s':_) = 
                let p = lsB s
                    l  = Line (p+ls2Normal s RightHanded)  (offset *^ normalized (lsDispl s))
                    l' = Line (p+ls2Normal s' RightHanded) (offset *^ normalized (lsDispl s'))
                    IIntersect p' = lineLine2Intersect l l'
                in p':f (tail segs)

-- | Outline 
outlinePath :: [OrientedPolygon Point2] -> ToolPath
outlinePath polys = concat $ map (extrudePolygon.fst) polys

-- | Clip a line with a set of polygons
clipLine :: [Polygon Point2] -> Line Point2 -> [LineSeg Point2]
clipLine polys line = 
        let inters = concat $ map (linePolygon2Crossings line) polys
            cmpInter (ax,ay) (bx,by) = case compare ax bx of 
                                            EQ -> compare ay by
                                            c  -> c
            sorted = sortBy cmpInter inters

            f :: [Point2] -> Bool -> [LineSeg Point2]
            f points@(a:b:_) fill = if fill then (LineSeg a b) : (f (tail points) False)
                                            else f (tail points) True
            f (a:[]) True = error $ "Unterminated line segment: "++show sorted
            f _ _ = []
        in f sorted True

-- | Figure out regions where infill is necessary
infillRegions :: [Polygon Point2] -> [Polygon Point2]
infillRegions = id

infillPattern :: Double -> Box Point2 -> [Line Point2]
infillPattern infillRatio (a,b) =
        map (\t -> Line (lerp a b t) (1,1)) ts
        where ts = map (/10) [0..20]

infillPath :: Double -> [OrientedPolygon Point2] -> ToolPath
infillPath infillRatio opolys = 
        let polys = map fst opolys
            regions = infillRegions polys
            bb = polygons2BoundingBox polys
            pattern = infillPattern infillRatio bb
            clipped = concat $ map (clipLine polys) pattern
        in concat $ map (\l->extrudeLineSegPath [l]) clipped


-- QuickCheck properties

-- | Properties for clipLine

