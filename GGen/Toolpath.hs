module GGen.Toolpath ( outlinePath
                     , infillPath
                     ) where

import Data.VectorSpace
import Data.List (foldl', sortBy)

import GGen.Geometry.Types
import GGen.Geometry.Polygon (polygonToLineSegPath)
import GGen.Geometry.Intersect (lineLineSeg2Intersect, lineLine2Intersect)
import GGen.Types

-- | Extrude path of line segments
extrudeLineSegPath :: LineSegPath Point2 -> ToolPath
extrudeLineSegPath path = XYMove (lsA $ head path) Dry : (map (\l -> XYMove (lsB l) (Extrude 1)) path)

-- | Extrude path outlining polygon
extrudePolygonPath :: Polygon Point2 -> ToolPath
extrudePolygonPath = extrudeLineSegPath . polygonToLineSegPath

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
outlinePath polys = concat $ map (extrudePolygonPath.fst) polys

-- | Clip a line with a set of polygons
clipLine :: [Polygon Point2] -> Line Point2 -> [LineSeg Point2]
clipLine polys line = 
        let segs = concat $ map polygonToLineSegPath polys
            inters = mapIntersection (lineLineSeg2Intersect line) segs
            cmpInter (ax,ay) (bx,by) = case compare ax bx of 
                                            EQ -> compare ay by
                                            c  -> c
            sorted = sortBy cmpInter inters
            f :: [Point2] -> Bool -> [LineSeg Point2]
            f points@(a:b:_) False = f (tail points) True
            f points@(a:b:_) True = (LineSeg a b) : (f (tail points) False)
            f _ True = error "Unterminated line segment"
            f _ False = []
        in f sorted True

-- | Figure out regions where infill is necessary
infillRegions :: [OrientedPolygon Point2] -> [Polygon Point2]
infillRegions polys = 
        undefined

infillPath :: Double -> [OrientedPolygon Point2] -> (ToolPath, Double)
infillPath infillRatio polys = 
        let regions = infillRegions polys
        in undefined

toolPath :: [OrientedPolygon Point2] -> ToolPath
toolPath = undefined

