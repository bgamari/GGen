{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies #-}

import Diagrams.Prelude hiding (Point(..), union, intersection)
import qualified Diagrams.Prelude as DP
import Diagrams.Backend.Cairo.CmdLine
import Data.Colour
  
import GGen.Geometry.Types
import GGen.Geometry.Polygon
import GGen.Geometry.PolygonCSG
import Control.Monad (forM_)
  
import Text.PrettyPrint (($$))
import qualified Text.PrettyPrint as PP

-- Trivial squares
a, b :: Polygon R2
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

-- Figure 2.1 from paper
c, d :: Polygon R2
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
    
p2p :: Point R2 -> DP.Point R2
p2p (P v) = DP.P v

polygonToPath (Polygon ps) = close $ fromVertices $ map p2p ps

edgeToPath :: (PathLike p, V p ~ R2) => LineSeg R2 -> p
edgeToPath (LineSeg a b) = fromVertices [p2p a, p2p b]

taggedEdgeToPath :: (HasStyle p, PathLike p, V p ~ R2) => (Edge,Tag) -> p
taggedEdgeToPath (edge, tag) =
  edgeToPath edge
  # lcA (color `withOpacity` 0.5)
  # lw 0.05
  where color = case tag of
                     Inside   -> blue
                     Outside  -> red
                     PosBound -> green
                     NegBound -> orange
        
--labelledTaggedEdge :: (Edge,Tag) -> a
labelledTaggedEdge (edge, tag) =
  taggedEdgeToPath (edge, tag)
  <> text tag' # translate (pos .-. P (0,0))
  where pos = alerp (lsA edge) (lsB edge) 0.5 .-^ 0.5 *^ ls2Normal edge RightHanded
        tag' = case tag of
          Inside   -> "I"
          Outside  -> "O"
          PosBound -> "+"
          NegBound -> "-"
         
prettySegmentation :: [Edge] -> [Edge] -> PP.Doc
prettySegmentation a b =
  PP.cat $ map (\l->PP.text (show l) 
                    $$ (PP.nest 2 $ PP.vcat $ map (PP.text . show) $ segmentEdge a l)
               ) b
  
main = do let segs = segmentBoth (polygonToLineSegPath c) (polygonToLineSegPath d)
          putStrLn "cd"
          print $ prettySegmentation (polygonToLineSegPath c) (polygonToLineSegPath d)
          putStrLn ""
          putStrLn "dc"
          print $ prettySegmentation (polygonToLineSegPath d) (polygonToLineSegPath c)
       
          --let segs = segmentEdge (polygonToLineSegPath d) (LineSeg (P (0,-8)) (P (0,8)))
          --        ++ segmentEdge (polygonToLineSegPath c) (LineSeg (P (0,-5)) (P (0,-3)))
          --        ++ segmentEdge (polygonToLineSegPath c) (LineSeg (P (0,5)) (P (0,3)))
          
          --print $ segmentEdge (polygonToLineSegPath d) (LineSeg (P (0,-8)) (P (0,8)))
          --print $ segmentEdge (polygonToLineSegPath c) (LineSeg (P (0,-5)) (P (0,-3)))
          --print $ segmentEdge (polygonToLineSegPath c) (LineSeg (P (0,5)) (P (0,3)))
       
          defaultMain $ mconcat $ map edgeToPath
                      $ (polygonToLineSegPath c) `union` (polygonToLineSegPath d)
          --defaultMain $ mconcat $ map labelledTaggedEdge segs

