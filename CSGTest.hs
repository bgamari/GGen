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

p2p (P v) = DP.P v
            
polygonToPath (Polygon ps) = close $ fromVertices $ map p2p ps

edgeToPath (LineSeg a b) = fromVertices [p2p a, p2p b]
                           
taggedEdgeToPath (edge, tag) = edgeToPath edge # lc color
    where color = case tag of
            Inside   -> blue
            Outside  -> red
            PosBound -> yellow
            NegBound -> orange
         
main = do let segs = segment (polygonToLineSegPath b) (polygonToLineSegPath a)
          --let segs = segmentEdge (polygonToLineSegPath b) (head $ polygonToLineSegPath a)
          print segs
          defaultMain $ mconcat $ map edgeToPath
                      $ (polygonToLineSegPath a) `difference` (polygonToLineSegPath b)
          --defaultMain $ mconcat $ map taggedEdgeToPath segs

