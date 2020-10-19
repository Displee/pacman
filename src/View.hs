module View where

import Model
import Controller

import Graphics.Gloss

view :: GameState -> IO Picture
view = return . viewPure

notPlayingGrid :: GameState -> Picture
notPlayingGrid (GameState (Maze w h l) status p) = Pictures (grid gridPaddingLeft gridPaddingTop w h)

viewPure :: GameState -> Picture
viewPure gs = case status gs of
  NotPlaying  -> notPlayingGrid gs
  Playing     -> color green (text "Playing!")
  GameOver    -> color green (text "Game over!")