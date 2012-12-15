-- Copyright (C) 2012 Jonathan Lamothe

-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see: http://www.gnu.org/licenses/

module Types
       ( GameState (..) 
       )
       where

import qualified Graphics.UI.SDL as SDL
import qualified System.Random as Rand

data GameState =
  GameState
  { surface :: SDL.Surface
  , gen :: Rand.StdGen
  }
