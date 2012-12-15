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
       , Object
       )
       where

import Control.Monad
import qualified Graphics.UI.SDL as SDL
import qualified System.Random as Rand

data GameState =
  GameState
  { surface :: SDL.Surface
  , audio :: Bool
  , gen :: Rand.StdGen
  } deriving Show

class Object o where
  getObjGeom :: o -> SDL.Rect
  setObjGeom :: SDL.Rect -> o -> o
  isObjVisible :: o -> Bool
  objIsVisible :: o -> o
  objIsInvisible :: o -> o
  isObjEnabled :: o -> Bool
  enableObj :: o -> o
  disableObj :: o -> o
  wasMouseOver :: o -> Bool
  mouseWasOver :: o -> o
  mouseWasNotOver :: o -> o
  isObjPressed :: o -> Bool
  objIsPressed :: o -> o
  objNotPressed :: o -> o
  onDrawObj :: SDL.Surface -> o -> IO ()
  onMouseOver :: o -> IO o
  onMouseOut :: o -> IO o
  onPress :: o -> IO o
  onRelease :: o -> IO o
  onClick :: o -> IO o

  onMouseOver = return
  onMouseOut = return
  onPress = return
  onRelease = return
  onClick = return

drawObj :: Object o => SDL.Surface -> o -> IO Bool
drawObj surf obj
  | isObjVisible obj = onDrawObj surf obj >> return True
  | otherwise = return False

handleObjEvent :: Object o => SDL.Event -> o -> IO o
handleObjEvent (SDL.MouseMotion x y _ _) obj
  | isObjEnabled obj =
    if wasMouseOver obj
    then
      if isInRect (getObjGeom obj) (fromIntegral x) (fromIntegral y)
      then return obj
      else onMouseOut $ mouseWasNotOver obj
    else
      if isInRect (getObjGeom obj) (fromIntegral x) (fromIntegral y)
      then onMouseOver $ mouseWasOver obj
      else return obj
  | otherwise = return obj
handleObjEvent _ obj = return obj

isInRect :: SDL.Rect -> Int -> Int -> Bool
isInRect (SDL.Rect rX rY w h) x y =
  x >= x1 && x <= x2 && y >= y1 && y <= y2
  where
    x1 = fromIntegral rX :: Int
    y1 = fromIntegral rY :: Int
    x2 = x1 + fromIntegral w
    y2 = y1 + fromIntegral h

-- jl
