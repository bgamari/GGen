module Data.STL ( Data.STL.parse
                , module Data.STL.Types
                ) where

import System.IO (readFile)
import Data.List (isPrefixOf)

import Data.STL.Types
import Data.STL.ASCII as A
import Data.STL.Binary as B

parse :: FilePath -> IO STLFile
parse filename = do a <- readFile filename
                    if "solid" `isPrefixOf` a then A.parse filename
                                              else B.parse filename
