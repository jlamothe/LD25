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
       , objDraw
       , Interactive (..)
       , interHandleEvent
       )
       where

import qualified Data.Map as Map
import Control.Monad
import qualified Graphics.UI.SDL as SDL
import qualified System.Random as Rand

data GameState =
  GameState
  { surface :: SDL.Surface
  , audio :: Bool
  , gen :: Rand.StdGen
  , playerPos :: Position
  , playerTile :: SDL.Surface
  , grassTile :: SDL.Surface
  , bldgTile :: SDL.Surface
  , roadIntTile :: SDL.Surface
  , roadHorizTile :: SDL.Surface
  , roadVertTile :: SDL.Surface
  , gameOver :: Bool
  } deriving Show

type Position = (Int, Int)

class Object o where
  objGetGeom :: o -> SDL.Rect
  objSetGeom :: SDL.Rect -> o -> o

  objIsVisible :: o -> Bool
  objIsVisible _ = True

  objSetVisible :: Bool -> o -> o
  objSetVisible _ obj = obj

  objOnDraw :: SDL.Surface -> o -> IO ()
  objOnDraw _ _ = return ()

objDraw :: Object o => SDL.Surface -> o -> IO Bool
objDraw surf obj
  | objIsVisible obj = objOnDraw surf obj >> return True
  | otherwise = return False

class Object i => Interactive i where
  interIsEnabled :: i -> Bool
  interIsEnabled _ = True

  interSetEnable :: Bool -> i -> i
  interSetEnable _ obj = obj

  interWasMouseOver :: i -> Maybe Bool
  interWasMouseOver _ = Nothing

  interSetMouseWasOver :: Bool -> i -> i
  interSetMouseWasOver _ obj = obj

  interIsPressed :: i -> Maybe Bool
  interIsPressed obj = Nothing

  interSetPressed :: Bool -> i -> i
  interSetPressed _ obj = obj

  interOnMouseOver :: GameState -> i -> (GameState, i)
  interOnMouseOver gs obj = (gs, obj)

  interOnMouseOut :: GameState -> i -> (GameState, i)
  interOnMouseOut gs obj = (gs, obj)

  interOnPress :: GameState -> i -> (GameState, i)
  interOnPress gs obj = (gs, obj)

  interOnRelease :: GameState -> i -> (GameState, i)
  interOnRelease gs obj = (gs, obj)

  interOnClick :: GameState -> i -> (GameState, i)
  interOnClick gs obj = (gs, obj)

interHandleEvent :: Interactive i
                    => GameState
                    -> SDL.Event
                    -> i
                    -> (GameState, i)
interHandleEvent gs (SDL.MouseMotion x y _ _) obj
  | interIsEnabled obj =
    case interWasMouseOver obj of
      Just True ->
        if isInObj obj x y
        then (gs, obj)
        else interOnMouseOut gs $ interSetMouseWasOver False obj
      Just False ->
        if isInObj obj x y
        then interOnMouseOver gs $ interSetMouseWasOver True obj
        else (gs, obj)
      Nothing -> (gs, obj)
  | otherwise = (gs, obj)
interHandleEvent gs (SDL.MouseButtonDown x y _) obj
  | interIsEnabled obj =
    if isInObj obj x y
    then interOnPress gs $ interSetPressed True obj
    else (gs, obj)
  | otherwise = (gs, obj)
interHandleEvent gs (SDL.MouseButtonUp x y _) obj
  | interIsEnabled obj && interIsPressed obj == Just True =
    if isInObj obj y x
    then interOnClick gs $ interSetPressed False obj
    else interOnRelease gs $ interSetPressed False obj
  | otherwise = (gs, obj)
interHandleEvent gs _ obj = (gs, obj)

isInObj :: (Object o, Integral i) => o -> i -> i -> Bool
isInObj obj x y = isInRect (objGetGeom obj) x y

isInRect :: Integral i => SDL.Rect -> i -> i -> Bool
isInRect (SDL.Rect rX rY w h) x y =
  x >= x1 && x <= x2 && y >= y1 && y <= y2
  where
    x1 = fromIntegral rX
    y1 = fromIntegral rY
    x2 = x1 + fromIntegral w
    y2 = y1 + fromIntegral h

-- jl
