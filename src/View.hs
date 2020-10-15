module View where

import Model
import Controller

import Graphics.Gloss

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gs = case status gs of
  NotPlaying  -> Pictures (grid 0 0 15 15)
  Playing     -> color green (text "Playing!")
  GameOver    -> color green (text "Game over!")