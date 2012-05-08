{-# LANGUAGE OverloadedStrings #-}
                
module GGen.GCode ( GCommand
                  , GCodeM
                  , comment
                  , command
                  ) where

import           Control.Monad.Trans.RWS
import           Data.Monoid
import           Data.Text.Lazy.Builder as TB

-- | `GCodeM c s` is an action to generate `GCommands` with configuration `c` and state `s`
type GCodeM c s = RWS c GCommand s
     
-- | A GCode command
type GCommand = Builder

command :: Builder -> GCodeM c s ()
command s = tell $ s<>"\n"
        
comment :: Builder -> GCodeM c s ()
comment s = tell $ "; "<>s<>"\n"

