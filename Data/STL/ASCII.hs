module Data.STL.ASCII (parse) where

import Types
import Numeric.LinearAlgebra
import Data.Attoparsec
import qualified Data.Attoparsec.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

data STLFile = STLFile { stlName :: String
                       , stlFacets :: [Face] }
                       deriving (Show, Eq)

sstring = string . BC.pack

facet = do sstring "facet normal "
           n <- vector
           C.skipSpace
           sstring "outer loop"
           C.skipSpace
           a <- vertex
           b <- vertex
           c <- vertex
           C.skipSpace
           sstring "endloop"
           C.skipSpace
           sstring "endfacet"
           C.skipSpace

           -- Some software leaves the normal vectors zeroed
           let normal = if norm2 n == 0 then normalize $ (b-a) `cross3` (c-a)
                                        else n
           return $ Face { faceNormal=normal
                         , faceVertices=(a,b,c) }
        where vector = do v <- count 3 (do a <- C.double
                                           C.char ' '
                                           return a)
                          return $ fromList v
              vertex = sstring "vertex " >> vector
           
stlFile = do sstring "solid"
             C.skipSpace
             name <- many (C.notChar '\n')
             facets <- many facet
             sstring $ "endsolid "++name
             C.skipSpace
             endOfInput
             return $ STLFile { stlName=name, stlFacets=facets }

parse :: FilePath -> IO STLFile
parse filename = do a <- B.readFile filename
                    return $ parse stlFile a

main = do print $ parse "z-tensioner_1off.stl"

