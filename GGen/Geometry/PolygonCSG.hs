module GGen.Geometry.PolygonCSG ( -- * Common operations
                                  Edge
                                , union
                                , intersection
                                , difference
                                , exclOr
                                  -- * Internals
                                , Tag(..)
                                , segment
                                ) where

import Data.Monoid
import Data.Maybe (mapMaybe)
import Data.List (sortBy, foldl', nubBy)
import Data.Function (on)
import Data.VectorSpace
import Data.AffineSpace
import GGen.Geometry.Types
import GGen.Geometry.Intersect

data Tag = Outside
         | Inside
         | PosBound
         | NegBound
         deriving (Show, Eq)

type Edge = LineSeg Vec2

-- | Klein four group binary operator
(*+*) :: Tag -> Tag -> Tag
Outside  *+* a        = a
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
segment :: [Edge] -> [Edge] -> [(Edge, Tag)]
segment a b = let a' = concat $ map (segmentEdge b) a
                  b' = concat $ map (segmentEdge a) b
              in a' ++ b'
          
-- | Segment and tag an edge against a polysolid
segmentEdge :: [Edge] -> Edge -> [(Edge, Tag)]
segmentEdge p l =
        let proj p' = ((p' .-. lsA l) <.> lsDispl l) / (magnitude $ lsDispl l)^2
            innerPoints :: [(Point Vec2, Tag)]
            innerPoints = sortBy (compare `on` (proj . fst))
                        $ mapMaybe (intersectPoint l) p
            tags = scanl (*+*) Outside $ map snd innerPoints
            points = sortBy (compare `on` proj)
                   $ [lsA l, lsB l] ++ map fst innerPoints
            edges = zipWith LineSeg points (tail points)
        in if null innerPoints
              then [(l, Outside)]
              else filter (\(e,_) -> not $ lsDispl e =~ 0)
                 $ filter (\(LineSeg a b,_) -> 0 <~ proj a && proj a <~ 1
                                            && 0 <~ proj b && proj b <~ 1)
                 $ zip edges tags

-- | Gets the location and tag of the intersection of two edges
intersectPoint :: Edge -> Edge -> Maybe (Point Vec2, Tag)
intersectPoint l e =
  case lineLineSeg2Intersect l' e of
    IIntersect i | i =~ lsA e ->  if lsDispl e <.> lsDispl l > 0
                                     then Just (i, PosBound)
                                     else Just (i, NegBound)
    IIntersect i | i =~ lsB e ->  if lsDispl e <.> lsDispl l > 0
                                     then Just (i, NegBound)
                                     else Just (i, PosBound)
    IIntersect i   ->  Just (i, Inside)
    IDegenerate    ->  Nothing
    INull          ->  Nothing
  where l' = Line (lsA l) (lsDispl l)

filterEdges :: Tag -> [(Edge,Tag)] -> [Edge]
filterEdges tag = map fst . filter (\(_,t) -> t == tag)
                  
union :: [Edge] -> [Edge] -> [Edge]
union p q = filterEdges Outside p'
         ++ filterEdges Outside q'
         ++ parEdges PosBound
         ++ parEdges NegBound
  where p' = segment p q
        q' = segment q p
        parEdges tag = concat $ map (\(a,b) -> [a,b])
                     $ filter (\(a,b) -> lsDispl a `parallel` lsDispl b)
                       [(a,b) | a <- filterEdges tag p'
                              , b <- filterEdges tag q']

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
