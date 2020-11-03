module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
          level <- readFile "./data/level_0.txt"
          let initialGameState = GameState (Maze 55 35 0 tiles) NotPlaying (Player (tileToScreenX 13) (tileToScreenY 12) (Tile 13 12 NormalTile) West Nothing undefined undefined undefined) undefined where
                                                          tiles = gridMaker 1 1 level
          playIO (InWindow "PacMan" (screenWidth, screenHeight) (0, 0)) -- Or FullScreen
                        black            -- Background color
                        60               -- Frames per second
                        initialGameState -- Initial state
                        view             -- View function
                        input            -- Event function
                        loop             -- The game loop function
