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
                     , gameOver = False
                     }

genGrassTile :: IO SDL.Surface
genGrassTile = genSolidTile 0 0x7f 0

genBldgTile :: IO SDL.Surface
genBldgTile = genSolidTile 0x7f 0x7f 0x7f

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
  event <- SDL.pollEvent
  let gs' = logic gs event
  if gameOver gs'
    then return gs'
    else mainLoop gs'

drawScreen :: GameState -> IO ()
drawScreen gs = do
  drawMap gs
  drawSprites gs
  SDL.flip $ surface gs

drawMap :: GameState -> IO ()
drawMap gs = mapM_ (drawMapRow gs) [(-15) .. 15]

drawMapRow :: Integral i => GameState -> i -> IO ()
drawMapRow gs y = mapM_ (flip (drawMapTile gs) y) [(-20) .. 20]

drawMapTile :: Integral i => GameState -> i -> i -> IO ()
drawMapTile gs x y =
  SDL.blitSurface tile Nothing (surface gs) (Just rect) >> return ()
  where
    tile = getRelMapTile gs x y
    rect = getMapRect x y

getRelMapTile :: Integral i => GameState -> i -> i -> SDL.Surface
getRelMapTile gs x y =
  getAbsMapTile gs x' y'
  where
    x' = fromIntegral pX + x
    y' = fromIntegral pY + y
    (pX, pY) = playerPos gs

getAbsMapTile :: Integral i => GameState -> i -> i -> SDL.Surface
getAbsMapTile gs x y =
  if x `mod` 6 == 0
  then
    if y `mod` 6 == 0
    then roadIntTile gs
    else roadVertTile gs
  else
    if y `mod` 6 == 0
    then roadHorizTile gs
    else
      if x `mod` 3 == 0 || y `mod` 3 == 0
      then grassTile gs
      else bldgTile gs

getMapRect :: Integral i => i -> i -> SDL.Rect
getMapRect x y = SDL.Rect x' y' 16 16
  where
    x' = fromIntegral $ originX + 16 * x
    y' = fromIntegral $ originY + 16 * y

originX :: Integral i => i
originX = (640 - 16) `div` 2

originY :: Integral i => i
originY = (480 - 16) `div` 2

drawSprites :: GameState -> IO ()
drawSprites gs =
  SDL.blitSurface tile Nothing surf origin >> return ()
  where
    tile = playerTile gs
    surf = surface gs
    origin = Just $ getMapRect 0 0

logic :: GameState -> SDL.Event -> GameState
logic gs (SDL.KeyDown k) = case SDL.symKey k of
  SDL.SDLK_UP -> moveUp gs
  SDL.SDLK_k -> moveUp gs
  SDL.SDLK_DOWN -> moveDown gs
  SDL.SDLK_j -> moveDown gs
  SDL.SDLK_LEFT -> moveLeft gs
  SDL.SDLK_h -> moveLeft gs
  SDL.SDLK_RIGHT -> moveRight gs
  SDL.SDLK_l -> moveRight gs
  SDL.SDLK_ESCAPE -> gs { gameOver = True }
  SDL.SDLK_q -> gs { gameOver = True }
  _ -> gs
logic gs SDL.Quit = gs { gameOver = True }
logic gs _ = gs


moveUp :: GameState -> GameState
moveUp gs = if isValidPos x' y'
            then gs { playerPos = (x', y') }
            else gs
  where
    x' = x
    y' = y - 1
    (x, y) = playerPos gs

moveDown :: GameState -> GameState
moveDown gs = if isValidPos x' y'
              then gs { playerPos = (x', y') }
              else gs
  where
    x' = x
    y' = y + 1
    (x, y) = playerPos gs

moveLeft :: GameState -> GameState
moveLeft gs = if isValidPos x' y'
              then gs { playerPos = (x', y') }
              else gs
  where
    x' = x - 1
    y' = y
    (x, y) = playerPos gs

moveRight :: GameState -> GameState
moveRight gs = if isValidPos x' y'
               then gs { playerPos = (x', y') }
               else gs
  where
    x' = x + 1
    y' = y
    (x, y) = playerPos gs

isValidPos :: Integral i => i -> i -> Bool
isValidPos x y = x `mod` 3 == 0 || y `mod` 3 == 0

cleanUp :: GameState -> IO ()
cleanUp gs = when (audio gs) Sound.closeAudio

-- jl
