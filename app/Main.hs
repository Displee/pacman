module Main where

import Controller
import Model
import View

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
          pacman <- loadBMP "data/ghost/pacmanright1.bmp"
          g1Icon <- loadBMP "data/ghost/pinky.bmp"
          g2Icon <- loadBMP "data/ghost/pinky.bmp"
          g3Icon <- loadBMP "data/ghost/pinky.bmp"
          g4Icon <- loadBMP "data/ghost/pinky.bmp"
          maze <- createMaze 55 35 0
          let g1 = Ghost (tileToScreenX 14) (tileToScreenY 15) g1Icon Pinky (Tile 14 15 NormalTile) West Nothing 1 Scatter 1 1 startGhostCageTicks
          let g2 = Ghost (tileToScreenX 14) (tileToScreenY 16) g2Icon Inky (Tile 14 16 NormalTile) West Nothing 1 Scatter 1 1 startGhostCageTicks
          let g3 = Ghost (tileToScreenX 15) (tileToScreenY 15) g3Icon Blinky (Tile 15 15 NormalTile) West Nothing 1 Scatter 1 1 startGhostCageTicks
          let g4 = Ghost (tileToScreenX 15) (tileToScreenY 16) g4Icon Clyde (Tile 15 16 NormalTile) West Nothing 1 Scatter 1 1 startGhostCageTicks
          let initialGameState = GameState maze NotPlaying (createPlayer pacman maze 15 14) [g1, g2, g3, g4] 0
          playIO (InWindow "PacMan" (screenWidth, screenHeight) (0, 0)) -- Or FullScreen
                        black            -- Background color
                        60               -- Frames per second
                        initialGameState -- Initial state
                        view             -- View function
                        input            -- Event function
                        loop             -- The game loop function

createMaze :: Int -> Int -> Int -> IO Maze
createMaze w h l = do
                      levelContent <- readFile ("./data/level_" ++ show l ++ ".txt")
                      return (Maze w h l (gridMaker 1 1 levelContent))

createPlayer :: Picture -> Maze -> Int -> Int -> Player
createPlayer icon m tx ty = Player icon (tileToScreenX tx) (tileToScreenY ty) tile West Nothing 1 0 3
                            where
                                  tile :: Tile
                                  tile = Tile tx ty NormalTile --TODO Get tile from maze