module View where

import Model

import Graphics.Gloss

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gs = case status gs of
  NotPlaying  -> color green (text "Not playing!")
  Playing     -> color green (text "Playing!")
  GameOver    -> color green (text "Game over!")