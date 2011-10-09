module Main where

import qualified GGen.Geometry.LineSeg as LS
import qualified GGen.Geometry.Intersect as I
import qualified GGen.Geometry.Types as T
import qualified GGen.Geometry.Polygon as P

main = do LS.runTests
          I.runTests
          T.runTests
          P.runTests

