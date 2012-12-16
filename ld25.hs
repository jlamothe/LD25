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
import qualified Graphics.UI.SDL.Image as SDLi
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
  pt <- SDLi.load "art/villain.png"
  gt <- genGrassTile
  bt <- genBldgTile
  rit <- genRoadIntTile
  rht <- genRoadHorizTile
  rvt <- genRoadVertTile
  return $ GameState { surface = s
                     , audio = a
                     , gen = g
                     , playerPos = (0, 0)
                     , playerTile = pt
                     , grassTile = gt
                     , bldgTile = bt
                     , roadIntTile = rit
                     , roadHorizTile = rht
                     , roadVertTile = rvt
                     }

genGrassTile :: IO SDL.Surface
genGrassTile = genSolidTile 0 0 0x7f

genBldgTile :: IO SDL.Surface
genBldgTile = genSolidTile 0xc0 0xc0 0xc0

genRoadIntTile :: IO SDL.Surface
genRoadIntTile = genSolidTile 0x3f 0x3f 0x3f

genRoadHorizTile :: IO SDL.Surface
genRoadHorizTile = do
  s <- genRoadIntTile
  p <- SDL.mapRGB (SDL.surfaceGetPixelFormat s) 0xff 0xff 0
  SDL.fillRect s (Just $ SDL.Rect 4 8 8 1) p
  return s

genRoadVertTile :: IO SDL.Surface
genRoadVertTile = do
  s <- genRoadIntTile
  p <- SDL.mapRGB (SDL.surfaceGetPixelFormat s) 0xff 0xff 0
  SDL.fillRect s (Just $ SDL.Rect 8 4 1 8) p
  return s

genSolidTile :: Integral i => i -> i -> i -> IO SDL.Surface
genSolidTile r g b = do
  s <- SDL.createRGBSurfaceEndian [] 16 16 32
  p <- SDL.mapRGB (SDL.surfaceGetPixelFormat s) r' g' b'
  SDL.fillRect s Nothing p
  return s
  where
    r' = fromIntegral r
    g' = fromIntegral g
    b' = fromIntegral b

mainLoop :: GameState -> IO GameState
mainLoop gs = do
  drawScreen gs
  SDL.pollEvent >>= logic gs

drawScreen :: GameState -> IO ()
drawScreen gs = do
  drawMap gs
  drawSprites gs
  SDL.flip $ surface gs

drawMap :: GameState -> IO ()
drawMap = undefined

drawSprites :: GameState -> IO ()
drawSprites = undefined

logic :: GameState -> SDL.Event -> IO GameState
logic = undefined

cleanUp :: GameState -> IO ()
cleanUp gs = when (audio gs) Sound.closeAudio

-- jl
