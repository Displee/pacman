module View where

import Model
import Controller

import Graphics.Gloss

view :: GameState -> IO Picture
view gs@(GameState _ _ (Player px py (Tile x y _) _ _ _ _ _ ) _) = do
                                pacman <-  loadBMP "pacmanright1.bmp"
                                let gridPicture = viewPure gs
                                return (Pictures [gridPicture, translate px py pacman])

drawTileGrid :: GameState -> Picture
drawTileGrid (GameState (Maze w h l t) status p _) = Pictures $ (grid gridPaddingLeft gridPaddingTop w h) ++ filled t
                                                             where
                                                                  tileColor t | t == NormalTile = black
                                                                              | t == Wall = blue
                                                                              | t == Dot || t == FlashingDot = black
                                                                  fillTile (Tile x y t) = fillCellSmart gridPaddingLeft gridPaddingTop x y (tileColor t)
                                                                  filled xs = map fillTile xs

viewPure :: GameState -> Picture
viewPure gs = case status gs of
  NotPlaying  -> drawTileGrid gs
  Playing     -> color green (text "Playing!")
  GameOver    -> color green (text "Game over!")
