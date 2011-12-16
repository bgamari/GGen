{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude hiding (P, union, intersection)
import qualified Diagrams.Prelude as DP
import Diagrams.Backend.Cairo.CmdLine
import GGen.Geometry.Types
import GGen.Geometry.Polygon
import GGen.Geometry.PolygonCSG

a, b :: Polygon Vec2
a = Polygon
    [ P ( 1, 1) 
    , P ( 1,-1)
    , P (-1,-1)
    , P (-1, 1)
    , P ( 1, 1)
    ]
b = Polygon
    [ P ( 0, 0) 
    , P ( 0, 2)
    , P ( 2, 2)
    , P ( 2, 0)
    , P ( 0, 0)
    ]

c = Polygon
    [ P ( 0, 8)
    , P (-8, 8)
    , P (-8,-8)
    , P ( 0,-8)
    ]
d = Polygon    
    [ P ( 0, 5)
    , P (-4, 5)
    , P (-4,-1)
    , P ( 2,-1)
    , P ( 2,-3)
    , P ( 0,-3)
    , P ( 0,-5)
    , P ( 4,-5)
    , P ( 4, 1)
    , P (-2, 1)
    , P (-2, 3)
    , P ( 0, 3)
    ]
    
p2p (P v) = DP.P v

polygonToPath (Polygon ps) = close $ fromVertices $ map p2p ps

edgeToPath (LineSeg a b) = fromVertices [p2p a, p2p b]
                           
taggedEdgeToPath (edge, tag) = edgeToPath edge # lc color
    where color = case tag of
            Inside   -> blue
            Outside  -> red
            PosBound -> yellow
            NegBound -> orange
         
main = do let segs = segment (polygonToLineSegPath c) (polygonToLineSegPath d)
          --let segs = segmentEdge (polygonToLineSegPath d) (LineSeg (P (0,-8)) (P (0,8)))
          print segs
          --defaultMain $ mconcat $ map edgeToPath
          --            $ (polygonToLineSegPath c) `union` (polygonToLineSegPath d)
          defaultMain $ mconcat $ map taggedEdgeToPath segs

