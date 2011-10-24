module Data.STL.Types (STLFile(..)) where

import GGen.Geometry.Types

data STLFile = STLFile { stlName :: String
                       , stlFacets :: [Face] }
                       deriving (Show)

