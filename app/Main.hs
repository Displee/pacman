module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
          playIO (InWindow "PacMan" (screenWidth, screenHeight) (0, 0)) -- Or FullScreen
                        black            -- Background color
                        60               -- Frames per second
                        initialState     -- Initial state
                        view             -- View function
                        input            -- Event function
                        loop             -- The game loop function
