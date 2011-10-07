module Data.STL.ASCII (Data.STL.ASCII.parse) where

import Data.VectorSpace
import Data.Cross
import Data.Attoparsec
import qualified Data.Attoparsec.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad (liftM)

import Data.STL.Types
import GGen.Geometry.Types

sstring = string . BC.pack

vector = do x <- entry
            y <- entry
            z <- entry
            return (x,y,z)
         where entry = C.char ' ' >> C.double

z=(0,0,0)
facet = do C.skipSpace
           sstring "facet normal"
           n <- vector
           C.skipSpace
           sstring "outer loop"
           a <- vertex
           b <- vertex
           c <- vertex
           C.skipSpace
           sstring "endloop"
           C.skipSpace
           sstring "endfacet"
           C.skipSpace

           ---- Some software leaves the normal vectors zeroed
           let normal = if magnitude n == 0
                           then normalized $ (b ^-^ a) `cross3` (c-a)
                           else n
           return $ Face { faceNormal=normal
                         , faceVertices=(a,b,c) }
        where vertex = C.skipSpace >> sstring "vertex" >> vector
           
stlFile = do sstring "solid"
             C.skipSpace
             name <- liftM BC.unpack $ C.takeTill (=='\n')
             facets <- many facet
             sstring $ "endsolid "++name
             C.skipSpace
             C.endOfInput
             return $ STLFile { stlName=name, stlFacets=facets }

parse :: FilePath -> IO STLFile
parse filename = do a <- B.readFile filename
                    case Data.Attoparsec.parseOnly stlFile a of
                         Right r  -> return r
                         Left e    -> error $ "Parsing error:\n"++e

