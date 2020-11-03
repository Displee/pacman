module View where

import Model
import Controller

import Graphics.Gloss

getGhostIcon :: Ghost -> Picture
getGhostIcon (Ghost gx gy gi _ _ _ _ _ _) = translate gx gy gi

view :: GameState -> IO Picture
view gs@(GameState _ _ (Player pi px py (Tile x y _) _ _ _ _ _ ) g) = do
                                let gridPicture = viewPure gs
                                let pacman = translate px py pi
                                let ghost1 = getGhostIcon $ head g
                                let ghost2 = getGhostIcon $ g!!1
                                let ghost3 = getGhostIcon $ g!!2
                                let ghost4 = getGhostIcon $ g!!3
                                return (Pictures [gridPicture, pacman, ghost1, ghost2, ghost3, ghost4]) -- TODO add ghosts

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
