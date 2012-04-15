{-# LANGUAGE PackageImports, TypeFamilies #-}

module GGen.ToolPath ( outlinePath
                     , toolPath
                     ) where

import Data.VectorSpace
import Data.AffineSpace
import Data.List (foldl', sortBy, deleteBy, partition)
import Data.Function (on)
import Control.Monad.Trans.State

import qualified Data.Sequence as S

import GGen.Geometry.Types
import GGen.Geometry.Clip
import GGen.Geometry.Polygon (polygonToLineSegPath, linePolygon2Crossings, offsetPolygon)
import GGen.Geometry.Intersect (lineLine2Intersect)
import GGen.Geometry.BoundingBox (polygons2BoundingBox)
import GGen.Types
import GGen.ToolPath.Infill

deleteFirst :: Eq a => Seq a -> a -> Seq a 
deleteFirst xs x = front >< S.drop 1 back 
        where (front, back) = S.break (==x) xs 

closestStart :: Point R2 -> ToolPath m t -> Maybe (Point R2)
closestStart p0 (ToolMove {tpMove=l}) =
             Just $ minimumBy (compare `on` distance p0) [lsA l, lsB l]
closestStart p0 (ToolPath Ordered s) =
             case viewl s of
                  a :< _     -> Just $ closestStart a
                  otherwise  -> Nothing
closestStart p0 (ToolPath Unordered s) =
             case S.filter isJust $ fmap (closestStart p0) s of
                  a | S.null a  -> Nothing
                  a             -> Just $ minimumBy (compare `on` distance p0) a
closestStart p0 (Marker _) = Nothing
           

-- | `popMin f xs` returns the minimum of `xs` under the comparison of
-- `fst $ map f xs`. It addition, it returns `xs` with the
-- corresponding minimal `x` removed. This is convenient in computing,
-- for instance, distance-optimal tool move scheduling
popMin :: Ord b => (a -> [(b,c)]) -> [a] -> ([a],c)
popMin f xs =
        let (x,_,c) = minimumBy (compare `on` (\(_,b,_)->b))
                      $ do x <- xs
                           (b,c) <- f x
                           return (x,b,c)
        in (xs // x, c)
         
concatToolPaths' :: ToolPath m t -> [ToolPath m t] -> ToolPath m t
concatToolPaths' tp [] = tp
concatToolPaths' tp tps =
        let p0 = tpEnd tp
            f tp = map (distance p0) [tp, tpInvert tp]
            (tps', tp') = popMin f tps
        in concatToolPaths (tp++tp') tps'
        
concatToolPaths :: [ToolPath m t] -> ToolPath m t
concatToolPaths tps = concatToolPaths' tps
                
-- | `flattenPath p0 tp` reduces a ToolPath `tp` to a minimal distance
-- OrderedPath from the given starting point `p0`
flattenPath :: P3 -> ToolPath m t -> ToolPath m t
flattenPath p0 tp = OrderedPath $ doPath p0 tp
        where doPath :: P3 -> ToolPath m t -> [ToolPath m t]
              doPath p0 tp@(ToolMove {}) = [tp]
              doPath p0 (ToolPath path) = path
              doPath p0 (UnorderedPath tps) =
                     let f tp = map (\x->(magnitude $ x .-. p0, x)) [tp, tpInvert tp]
                         (tps', tp') = popMin f tps
                     in tps'
              

-- | Extrude path of line segments
extrudeLineSegPath :: LineSegPath R2 -> ToolPath m t
extrudeLineSegPath = OrderedPath . map (\l -> ToolMove l (Extrude 1))

-- | Extrude path outlining polygon
extrudePolygon :: Polygon R2 -> ToolPath
extrudePolygon = extrudeLineSegPath . polygonToLineSegPath

-- | Build the toolpath describing the outline of a slice 
outlinePath :: [OrientedPolygon R2] -> ToolPath
outlinePath polys = concat $ map (extrudePolygon.fst) polys

-- | Build the toolpaths of a stack of slices
toolPath :: InfillPattern s -> InfillPattern t -> Slice -> State (s,t) ToolPath
toolPath intPattern extPattern (_,opolys) = 
        do (intState, extState) <- get
           let (intPolys, extPolys) = partition (\(p,exposure) -> exposure==Internal) opolys
               (intInfill, intState') = runState (infillPathM intPattern (map fst intPolys)) intState
               (extInfill, extState') = runState (infillPathM extPattern (map fst extPolys)) extState
           put (intState', extState')
           return $ concatToolPaths [outlinePath (map fst opolys), intInfill, extInfill]

