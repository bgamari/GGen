{-# LANGUAGE PackageImports, TypeFamilies #-}

module GGen.ToolPath ( outlinePath
                     , infillPathM
                     , toolPath
                     , HexInfill(..)
                     , initialState
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

class InfillGenerator t where
        type IState t :: *
        initialState :: t -> IState t
        pattern :: t -> Box Vec2 -> State (IState t) [Line Vec2]

data HexInfill = HexInfill { infillRatio :: Double } deriving (Show)

instance InfillGenerator HexInfill where
        type IState HexInfill = [Angle]
        initialState _ = cycle [0, 60, 120]
        pattern a = polygonInfillPattern (infillRatio a)

polygonInfillPattern :: InfillRatio -> Box Vec2 -> State [Angle] [Line Vec2]
polygonInfillPattern infillRatio (a,b) =
     do infillAngle:rest <- get
        put rest
        let ts = map (/40) [-40..40]
            phi = infillAngle / 180 * pi
            lBegin = alerp a (a .+^ magnitude (b.-.a) *^ (-sin phi, cos phi))
        return $ map (\t -> Line (lBegin t) (cos phi, sin phi)) ts

-- | Build the toolpath describing the infill of a slice
infillPathM :: InfillGenerator a => a -> [OrientedPolygon Vec2] -> State (IState a) ToolPath
infillPathM infillGen opolys = 
        do let polys = map fst opolys
               bb = polygons2BoundingBox polys
           pat <- pattern infillGen bb
           let clipped = concat $ map (clipLine polys) pat
           return $ concatToolPaths $ map (\l->extrudeLineSegPath [l]) clipped

-- | Build the toolpaths of a stack of slices
toolPath :: InfillGenerator a => a -> Slice -> State (IState a) ToolPath
toolPath infillGen (_,opolys) = 
        do infill <- infillPathM infillGen opolys
           return $ concatToolPaths [outlinePath opolys, infill]

-- QuickCheck properties

-- | Properties for clipLine

