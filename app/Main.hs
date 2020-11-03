module Main where

import Controller
import Model
import View

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
          level <- readFile "./data/level_0.txt"
          pacman <- loadBMP "data/ghost/pacmanright1.bmp"
          g1Icon <- loadBMP "data/ghost/pinky.bmp"
          g2Icon <- loadBMP "data/ghost/pinky.bmp"
          g3Icon <- loadBMP "data/ghost/pinky.bmp"
          g4Icon <- loadBMP "data/ghost/pinky.bmp"
          let g1 = Ghost (tileToScreenX 14) (tileToScreenY 15) g1Icon Pinky (Tile 14 15 NormalTile) West 1 Scatter (Tile 0 1 NormalTile)
          let g2 = Ghost (tileToScreenX 14) (tileToScreenY 16) g2Icon Inky (Tile 14 16 NormalTile) West 1 Scatter (Tile 0 1 NormalTile)
          let g3 = Ghost (tileToScreenX 15) (tileToScreenY 15) g3Icon Blinky (Tile 15 15 NormalTile) West 1 Scatter (Tile 0 1 NormalTile)
          let g4 = Ghost (tileToScreenX 15) (tileToScreenY 16) g4Icon Clyde (Tile 15 16 NormalTile) West 1 Scatter (Tile 0 1 NormalTile)
          let initialGameState = GameState (Maze 55 35 0 tiles) NotPlaying (Player pacman (tileToScreenX 15) (tileToScreenY 12) (Tile 15 12 NormalTile) West Nothing undefined undefined undefined) [g1, g2, g3, g4]
                                 where
                                      tiles = gridMaker 1 1 level
          playIO (InWindow "PacMan" (screenWidth, screenHeight) (0, 0)) -- Or FullScreen
                        black            -- Background color
                        60               -- Frames per second
                        initialGameState -- Initial state
                        view             -- View function
                        input            -- Event function
                        loop             -- The game loop function
