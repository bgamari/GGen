module GGen.Geometry.Clip (clipLine) where                

import Data.List
import GGen.Geometry.Intersect
import GGen.Geometry.Polygon
import GGen.Geometry.Types

-- | Clip a line with a set of polygons
clipLine :: [Polygon R2] -> Line R2 -> [LineSeg R2]
clipLine polys line = 
        let inters = concat $ map (linePolygon2Crossings line) polys
            cmpInter a b = let (ax,ay) = unp2 a
                               (bx,by) = unp2 b
                           in case compare ax bx of 
                                   EQ -> compare ay by
                                   c  -> c
            sorted = sortBy cmpInter inters

            f :: [P2] -> Bool -> [LineSeg R2]
            f points@(a:b:_) fill = if fill then (LineSeg a b) : (f (tail points) False)
                                            else f (tail points) True
            f (a:[]) True = error $ "Unterminated line segment from possible points "++show sorted++" while clipping "++show line++" against polygons "++show polys
            f _ _ = []
        in f sorted True