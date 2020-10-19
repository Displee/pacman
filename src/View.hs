module View where

import Model
import Controller

import Graphics.Gloss

view :: GameState -> IO Picture
view = return . viewPure

--TODO Read this from a file
level0 = "##################################################\n\
          \#                                                #\n\
          \#                                                #\n\
          \#                                                #\n\
          \#                                                #\n\
          \#####                                        #####\n\
          \    #                                        #    \n\
          \#####                                        #####\n\
          \                                                  \n\
          \#####                                        #####\n\
          \    #                                        #    \n\
          \#####                                        #####\n\
          \#                                                #\n\
          \#                                                #\n\
          \#                                                #\n\
          \#                                                #\n\
          \##################################################"

initialState :: GameState
initialState = GameState (Maze 55 25 0 tiles) NotPlaying (Player (Tile 3 3 NormalTile)) where
               tiles = gridMaker 1 1 level0

drawTileGrid :: GameState -> Picture
drawTileGrid (GameState (Maze w h l t) status p) = Pictures $ (grid gridPaddingLeft gridPaddingTop w h) ++ filled t
                                                   where
                                                        tileColor t | t == NormalTile = black
                                                                    | t == Wall = blue
                                                                    | t == Dot || t == FlashingDot = red
                                                        fillTile (Tile x y t) = fillCellSmart gridPaddingLeft gridPaddingTop x y (tileColor t)
                                                        filled xs = map fillTile xs

viewPure :: GameState -> Picture
viewPure gs = case status gs of
  NotPlaying  -> drawTileGrid gs
  Playing     -> color green (text "Playing!")
  GameOver    -> color green (text "Game over!")