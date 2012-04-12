module GGen.Geometry.Face ( moveVertex
                          ) where

import Data.VectorSpace
import GGen.Geometry.Types

-- | If the given point v is a vertex of the face, move it to v'. Otherwise
-- return the original face.
moveVertex :: Face -> P3 -> P3 -> Face
moveVertex (Face n (a,b,c)) v v' = let a' = if a =~ v then v' else a
                                       b' = if b =~ v then v' else b
                                       c' = if c =~ v then v' else c
                                   in Face n (a',b',c')
