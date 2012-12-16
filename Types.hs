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
       , Object (..)
       , drawObj
       , handleObjEvent
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
  wasMouseOverObj :: o -> Bool
  mouseWasOverObj :: o -> o
  mouseWasNotOverObj :: o -> o
  isObjPressed :: o -> Bool
  objIsPressed :: o -> o
  objNotPressed :: o -> o
  onDrawObj :: SDL.Surface -> o -> IO ()
  onMouseOverObj :: GameState -> o -> (GameState, o)
  onMouseOutObj :: GameState -> o -> (GameState, o)
  onPressObj :: GameState -> o -> (GameState, o)
  onReleaseObj :: GameState -> o -> (GameState, o)
  onClickObj :: GameState -> o -> (GameState, o)

  onMouseOverObj gs obj = (gs, obj)
  onMouseOutObj gs obj = (gs, obj)
  onPressObj gs obj = (gs, obj)
  onReleaseObj gs obj = (gs, obj)
  onClickObj gs obj = (gs, obj)

drawObj :: Object o => SDL.Surface -> o -> IO Bool
drawObj surf obj
  | isObjVisible obj = onDrawObj surf obj >> return True
  | otherwise = return False

handleObjEvent :: Object o
                  => GameState
                  -> SDL.Event
                  -> o
                  -> (GameState, o)
handleObjEvent gs (SDL.MouseMotion x y _ _) obj
  | isObjEnabled obj =
    if wasMouseOver obj
    then
      if isInObj obj x y
      then (gs, obj)
      else onMouseOutObj gs $ mouseWasNotOverObj obj
    else
      if isInObj obj x y
      then onMouseOverObj gs $ mouseWasOverObj obj
      else (gs, obj)
  | otherwise = (gs, obj)
handleObjEvent gs (SDL.MouseButtonDown x y _) obj
  | isObjEnabled obj =
    if isInObj obj x y
    then onPress gs $ objIsPressed obj
    else (gs, obj)
  | otherwise = (gs, obj)
handleObjEvent gs (SDL.MouseButtonUp x y _) obj
  | isObjEnabled obj && isObjPressed obj =
    if isInObj obj y x
    then onClick gs $ objNotPressed obj
    else onRelease gs $ objNotPressed obj
  | otherwise = (gs, obj)
handleObjEvent gs _ obj = (gs, obj)

isInObj :: (Object o, Integral i) => o -> i -> i -> Bool
isInObj obj x y = isInRect (getObjGeom obj) x y

isInRect :: Integral i => SDL.Rect -> i -> i -> Bool
isInRect (SDL.Rect rX rY w h) x y =
  x >= x1 && x <= x2 && y >= y1 && y <= y2
  where
    x1 = fromIntegral rX
    y1 = fromIntegral rY
    x2 = x1 + fromIntegral w
    y2 = y1 + fromIntegral h

-- jl
