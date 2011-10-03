module Data.STL.Binary (parse, STLFile(..)) where

import GGen.Types
import Numeric.LinearAlgebra
import Data.Binary.Get
import Data.Binary.IEEE754
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BC

data STLFile = STLFile { stlName :: String
                       , stlFacets :: [Face] }
                       deriving (Show, Eq)

vector = do x <- getFloat32le
            y <- getFloat32le
            z <- getFloat32le
            return $ fromList $ map realToFrac [x,y,z]

facet = do normal <- vector
           a <- vector
           b <- vector
           c <- vector
           attrs <- getWord16le
           return $ Face { faceNormal=normal, faceVertices=(a,b,c) }

stlFile = do name <- getBytes 80
             nFacets <- getWord32le
             facets <- mapM (const facet) [0..nFacets-1]
             return $ STLFile { stlName=BC.unpack name, stlFacets=facets }

parse :: FilePath -> IO STLFile
parse filename = do a <- B.readFile filename
                    return $ runGet stlFile a

main = do a <- parse "z-tensioner_1off.stl"
          print a

