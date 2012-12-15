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

import Control.Monad
import qualified System.Random as Rand
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Mixer as Sound
import Types

main = SDL.withInit [SDL.InitEverything] $
       initGame >>= mainLoop >>= cleanUp

initGame :: IO GameState
initGame = do
  s <- SDL.setVideoMode 640 480 32 [SDL.Fullscreen]
  a <- Sound.tryOpenAudio
       Sound.defaultFrequency
       Sound.AudioS16Sys
       2
       1024
  g <- Rand.getStdGen
  return $ GameState { surface = s, audio = a, gen = g }

mainLoop :: GameState -> IO GameState
mainLoop gs = do
  drawScreen gs
  SDL.pollEvent >>= logic gs

drawScreen :: GameState -> IO ()
drawScreen gs = undefined

logic :: GameState -> SDL.Event -> IO GameState
logic = undefined

cleanUp :: GameState -> IO ()
cleanUp gs = when (audio gs) Sound.closeAudio

-- jl
