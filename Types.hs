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
  wasMouseOver :: o -> Bool
  mouseWasOver :: o -> o
  mouseWasNotOver :: o -> o
  isObjPressed :: o -> Bool
  objIsPressed :: o -> o
  objNotPressed :: o -> o
  onDrawObj :: SDL.Surface -> o -> IO ()
  onMouseOver :: GameState -> o -> IO (GameState, o)
  onMouseOut :: GameState -> o -> IO (GameState, o)
  onPress :: GameState -> o -> IO (GameState, o)
  onRelease :: GameState -> o -> IO (GameState, o)
  onClick :: GameState -> o -> IO (GameState, o)

  onMouseOver gs obj = return (gs, obj)
  onMouseOut gs obj = return (gs, obj)
  onPress gs obj = return (gs, obj)
  onRelease gs obj = return (gs, obj)
  onClick gs obj = return (gs, obj)

drawObj :: Object o => SDL.Surface -> o -> IO Bool
drawObj surf obj
  | isObjVisible obj = onDrawObj surf obj >> return True
  | otherwise = return False

handleObjEvent :: Object o 
                  => GameState 
                  -> SDL.Event 
                  -> o 
                  -> IO (GameState, o)
handleObjEvent gs (SDL.MouseMotion x y _ _) obj
  | isObjEnabled obj =
    if wasMouseOver obj
    then
      if isInObj obj x y
      then return (gs, obj)
      else onMouseOut gs $ mouseWasNotOver obj
    else
      if isInObj obj x y
      then onMouseOver gs $ mouseWasOver obj
      else return (gs, obj)
  | otherwise = return (gs, obj)
handleObjEvent gs (SDL.MouseButtonDown x y _) obj
  | isObjEnabled obj =
    if isInObj obj x y
    then onPress gs $ objIsPressed obj
    else return (gs, obj)
  | otherwise = return (gs, obj)
handleObjEvent gs _ obj = return (gs, obj)

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
