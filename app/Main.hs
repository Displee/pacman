module Main where

import Controller
import Model
import View

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
          initialGameState <- createGameState 0
          playIO (InWindow "PacMan" (screenWidth, screenHeight) (0, 0)) -- Or FullScreen
                        black            -- Background color
                        60               -- Frames per second
                        initialGameState -- Initial state
                        view             -- View function
                        input            -- Event function
                        loop             -- The game loop function