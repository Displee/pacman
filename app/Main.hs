module Main where

import Controller
import Model
import View

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
          pacman <- loadBMP "data/ghost/pacmanright1.bmp"
          pinkyg <- loadBMP "data/PinkGhost/GhostPinkWest1.bmp"
          inkyg <- loadBMP "data/ghost/pinky.bmp"
          blinkyg <- loadBMP "data/ghost/ghostred.bmp"
          clydeg <- loadBMP "data/ghost/ghostblue.bmp"
          icons   <- pacmanviewer
          pinkghosticons <- pinkGhostviewer
          blueghosticons <-blueGhostviewer
          yellowghosticons <- yellowGhostviewer
          redghosticons <- redGhostviewer
          maze <- createMaze 55 35 0
          let pinky = Ghost (tileToScreenX 14) (tileToScreenY 15) pinkyg pinkghosticons Pinky (Tile 14 15 NormalTile) West West Nothing 1 Chase 1 1 startGhostCageTicks
          let inky = Ghost (tileToScreenX 14) (tileToScreenY 16)  inkyg blueghosticons Inky (Tile 14 16 NormalTile) West West Nothing 1 Chase 1 1 startGhostCageTicks
          let blinky = Ghost (tileToScreenX 15) (tileToScreenY 17)  blinkyg redghosticons Blinky (Tile 15 15 NormalTile) West West Nothing 1 Chase 1 1 startGhostCageTicks
          let clyde = Ghost (tileToScreenX 15) (tileToScreenY 16)  clydeg yellowghosticons Clyde (Tile 15 16 NormalTile) West West Nothing 1 Chase 1 1 startGhostCageTicks
          let initialGameState = GameState maze Starting (createPlayer pacman icons maze 15 26) [pinky, inky, blinky, clyde] 0
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

createPlayer :: Picture -> [Animation] -> p -> Int -> Int -> Player
createPlayer icon icons m tx ty = Player icon icons (tileToScreenX tx) (tileToScreenY ty) tile West Nothing 1 0 3
                            where
                                  tile :: Tile
                                  tile = Tile tx ty NormalTile --TODO Get tile from maze