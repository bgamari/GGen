module Numeric.LinearAlgebra.Utils ( cross3
                                   , normalize
                                   ) where

import Numeric.LinearAlgebra

cross3 a b = fromList [ (a @> 1)*(b @> 2) - (a @> 2)*(b @> 1)
                      , (a @> 2)*(b @> 0) - (a @> 0)*(b @> 2)
                      , (a @> 0)*(b @> 1) - (a @> 1)*(b @> 0) ]

normalize a = (1/norm2 a) `scale` a

