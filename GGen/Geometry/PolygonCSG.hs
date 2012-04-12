module GGen.Geometry.PolygonCSG ( -- * Common operations
                                  Edge
                                , union
                                , intersection
                                , difference
                                , exclOr
                                  -- * Internals
                                , Tag(..)
                                , segment, segmentBoth
                                , segmentEdge
                                ) where

import Data.Monoid
import Control.Monad (guard)
import Data.Maybe (mapMaybe)
import Data.List (sortBy, foldl', nubBy)
import Data.Function (on)
import Data.VectorSpace
import Data.AffineSpace
import GGen.Geometry.Types
import GGen.Geometry.Intersect
  
import Debug.Trace

data Tag = Outside
         | Inside
         | PosBound
         | NegBound
         deriving (Show, Eq)

type Edge = LineSeg R2

-- | Klein four group binary operator
(*+*) :: Tag -> Tag -> Tag
Outside  *+* a        =a
a        *+* b | a==b = Outside
Inside   *+* NegBound = PosBound
Inside   *+* PosBound = NegBound
NegBound *+* PosBound = Inside
a        *+* b        = b *+* a

-- | Klein four group binary operator
instance Monoid Tag where
        mempty = Outside
        mappend = (*+*)

-- | Segment and tag two polysolids
segment, segmentBoth :: [Edge] -> [Edge] -> [(Edge, Tag)]
segment a b = concat $ map (segmentEdge b) a
segmentBoth a b = segment a b ++ segment b a

invertTaggedEdge :: (Edge, Tag) -> (Edge, Tag)
invertTaggedEdge = id
--invertTaggedEdge (e,t) = (lsInvert e, Inside *+* t)

orientEdge :: (Edge, Tag) -> (Edge, Tag)
orientEdge te@(e,t)
  | l <.> (1,0) < 0  = invertTaggedEdge te
  | l <.> (1,0) == 0 && l <.> (0,1) < 0  = invertTaggedEdge te
  | otherwise  = te
  where l = lsDispl e

-- | Segment and tag an edge against a polysolid
segmentEdge :: [Edge] -> Edge -> [(Edge, Tag)]
segmentEdge p l
  | lsDispl l <.> (1,0) < 0  = segmentEdge p (lsInvert l)
  | lsDispl l <.> (1,0) == 0 && lsDispl l <.> (0,1) < 0  = segmentEdge p (lsInvert l)
  | otherwise = 
        let proj p' = ((p' .-. lsA l) <.> lsDispl l) / (magnitude $ lsDispl l)^2
            innerPoints :: [(Point R2, Tag)]
            innerPoints = sortBy (compare `on` (proj . fst))
                        $ mapMaybe (intersectPoint l) p
            tags = scanl (*+*) Outside $ map snd innerPoints
            points = sortBy (compare `on` proj)
                   $ [lsA l, lsB l] ++ map fst innerPoints
            edges = zipWith LineSeg points (tail points)
        in map orientEdge $
           if null innerPoints
              then [(l, Outside)]
              else filter (\(e,_) -> not $ magnitude (lsDispl e) =~ 0)
                 $ filter (\(LineSeg a b,_) -> 0 <~ proj a && proj a <~ 1
                                            && 0 <~ proj b && proj b <~ 1)
                 $ zip edges tags

-- | Gets the location and tag of the intersection of two edges
intersectPoint :: Edge -> Edge -> Maybe (Point R2, Tag)
intersectPoint l e =
  case lineLineSeg2Intersect l' e of
    IIntersect i | i =~ lsA e ->  if lsDispl e <.> ls2Normal l LeftHanded > 0
                                     then Just (i, PosBound)
                                     else Just (i, NegBound)
    IIntersect i | i =~ lsB e ->  if lsDispl e <.> ls2Normal l LeftHanded < 0
                                     then Just (i, PosBound)
                                     else Just (i, NegBound)
    IIntersect i   ->  Just (i, Inside)
    IDegenerate    ->  Nothing
    INull          ->  Nothing
  where l' = Line (lsA l) (lsDispl l)

-- | Filter for edges of a given tag
filterEdges :: Tag -> [(Edge,Tag)] -> [Edge]
filterEdges tag = map fst . filter (\(_,t) -> t == tag)
                  
union :: [Edge] -> [Edge] -> [Edge]
union p q = filterEdges Outside p'
         ++ filterEdges Outside q'
         ++ parEdges PosBound
         ++ parEdges NegBound
  where p' = segment p q
        q' = segment q p
        parEdges tag = traceShow tag $ traceShow (filterEdges NegBound q') $ trList $ concat
                       $ (do a <- filterEdges tag p'
                             b <- filterEdges tag q'
                             guard $ lsDispl a `sameDir` lsDispl b
                             guard $ IDegenerate == lineSegLineSeg2Intersect a b
                             return $ tr show [a,b])
        
tr :: (x -> String) -> x -> x
tr f x = trace (f x) x
trList = tr (unlines . map show)

intersection :: [Edge] -> [Edge] -> [Edge]
intersection p q  = filterEdges Inside p'
                 ++ filterEdges Inside q'
                 ++ nubBy approx (filterEdges PosBound p' ++ filterEdges PosBound q')
  where p' = segment p q
        q' = segment q p

difference :: [Edge] -> [Edge] -> [Edge]
difference p q  = filterEdges Outside p'
               ++ map lsInvert (filterEdges Inside q')
               ++ nubBy approx ( filterEdges NegBound p'
                              ++ filterEdges PosBound q')
  where p' = segment p q
        q' = segment q p

exclOr :: [Edge] -> [Edge] -> [Edge]
exclOr p q = (p `difference` q) `union` (q `difference` p)

