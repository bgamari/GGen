{-# LANGUAGE PatternGuards, PackageImports, TypeFamilies #-}

module GGen.ToolPath ( outlinePath
                     , toolPath
                     , extrudeLineSegPath
                     , extrudePolygon
                     , flattenPath
                     , concatOrdered, concatUnordered
                     ) where

import           Control.Monad (join)
import qualified Control.Monad.Trans.State as S
import qualified Control.Monad.Trans.RWS as RWS
import           Data.AffineSpace
import           Data.Foldable
import           Data.Function (on)
import           Data.List (partition, foldl', (\\))
import           Data.Maybe
import           Data.Monoid
import           Data.Traversable
import           Data.VectorSpace
import           Prelude hiding (mapM, mapM_)

import           Data.Sequence (Seq, ViewL(..), ViewR(..), (><))
import qualified Data.Sequence as SQ

import           GGen.Geometry.Types
import           GGen.Geometry.Clip
import           GGen.Geometry.Polygon (polygonToLineSegPath, linePolygon2Crossings, offsetPolygon)
import           GGen.Geometry.Intersect (lineLine2Intersect)
import           GGen.Geometry.BoundingBox (polygons2BoundingBox)
import           GGen.Types
import           GGen.ToolPath.Infill
         
findR :: (a -> Bool) -> Seq a -> Maybe a
findR f xs | xs' :> x <- SQ.viewr xs, f x = Just x
findR f _ = Nothing

findL :: (a -> Bool) -> Seq a -> Maybe a
findL f xs | x :< xs' <- SQ.viewl xs, f x = Just x
findL f _ = Nothing

-- | For each element in a sequence, return the element and the
-- sequence with the element removed.
removeElement :: Seq a -> [(a, Seq a)]
removeElement = 
  let removeElement' :: Seq a -> Seq a -> [(a, Seq a)]
      removeElement' past xs = let x SQ.:< rest = SQ.viewl xs
                               in (x, past <> rest) : removeElement' (past SQ.|> x) rest
  in removeElement' SQ.empty
        
-- | `flattenPath p0 tp` reduces a ToolPath `tp` to a minimal distance
-- OrderedPath from the given starting point `p0`
flattenPath :: P2 -> ToolPath m t -> Seq (ToolMove m t)
flattenPath p0 tp = snd $ RWS.evalRWS (doPath tp) () p0
        where --doPath :: ToolPath m t -> RWS.RWS () (Seq (ToolMove m t)) P2 ()
              doPath (PathStep tm@(ToolMove {tmMove=l})) = do RWS.put $ lsB l
                                                              RWS.tell $ SQ.singleton tm -- TODO: Invert
              doPath (ToolPath Ordered path) = mapM_ doPath path
              doPath (ToolPath Unordered path) = do
                     p0 <- RWS.get
                     as <- forM (removeElement path) $ \(tp', path')->do
                        let (_, w) = RWS.evalRWS (doPath tp') () p0
                        p1 <- RWS.get
                        return (tp', p1, w, path')

                     let --dist :: (ToolPath m t, P2, Seq (ToolMove m t)) -> Double
                         dist (tp, p1, w, path') | SQ.null w = 0
                                                 | otherwise = distance p0 
                                                               $ lsA $ tmMove
                                                               $ head $ toList w
                         (tp, p2, w, path') = minimumBy (compare `on` dist) as
                     RWS.put p2
                     doPath $ ToolPath Unordered path'
              doPath x = return ()
              
              
tpBegin :: ToolPath m t -> Maybe P2
tpBegin (ToolPath _ path) = join $ find isJust $ fmap tpBegin path
tpBegin t@(PathStep (ToolMove {tmMove=l})) = Just $ lsA l
tpBegin _ = Nothing

tpEnd :: ToolPath m t -> Maybe P2
tpEnd (ToolPath _ path) = join $ find isJust $ fmap tpEnd path
tpEnd t@(PathStep (ToolMove {tmMove=l})) = Just $ lsB l
tpEnd _ = Nothing

tpInvert :: ToolPath m t -> ToolPath m t
tpInvert (ToolPath Unordered path) = ToolPath Unordered $ fmap tpInvert path
tpInvert (ToolPath Ordered path) = ToolPath Ordered $ SQ.reverse $ fmap tpInvert path
tpInvert (PathStep t@(ToolMove {tmMove=l})) = PathStep (t { tmMove=lsInvert l })
tpInvert a = a

-- | Extrude path of line segments
extrudeLineSegPath :: PreserveOrder -> t -> LineSegPath R2 -> ToolPath m t
extrudeLineSegPath o t = ToolPath o . SQ.fromList . map (\l -> PathStep $ ToolMove l t)

-- | Extrude path outlining polygon
extrudePolygon :: t -> Polygon R2 -> ToolPath m t
extrudePolygon t = extrudeLineSegPath Ordered t . polygonToLineSegPath

-- | Build the toolpath describing the outline of a slice
outlinePath :: t -> [OrientedPolygon R2] -> ToolPath m t
outlinePath t = ToolPath Unordered . SQ.fromList . map (extrudePolygon t . fst)

-- | Build the toolpath for a slice
toolPath :: t -> InfillPattern s -> InfillPattern t -> Slice -> S.State (s,t) (ToolPath m t)
toolPath t intPattern extPattern (_,opolys) = 
        do (intState, extState) <- S.get
           let (intPolys, extPolys) = partition (\(p,exposure) -> exposure==Internal) opolys
               (intInfill, intState') = S.runState (infillPathM t intPattern (map fst intPolys)) intState
               (extInfill, extState') = S.runState (infillPathM t extPattern (map fst extPolys)) extState
           S.put (intState', extState')
           return $ ToolPath Unordered $ SQ.fromList [outlinePath t (map fst opolys), intInfill, extInfill]

concatOrdered :: ToolPath m t -> ToolPath m t -> ToolPath m t
(ToolPath Ordered a) `concatOrdered` (ToolPath Ordered b) = ToolPath Ordered $ a >< b
a `concatOrdered` b = ToolPath Ordered $ SQ.fromList [a,b]

concatUnordered :: ToolPath m t -> ToolPath m t -> ToolPath m t
a `concatUnordered` b = ToolPath Unordered $ SQ.fromList [a,b]

