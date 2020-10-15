module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle the game loop
loop :: Float -> GameState -> IO GameState
loop seconds gstate = do
                        return gstate

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate = gstate
inputKey _ gstate = gstate -- Otherwise keep the same

-- | Grid functions

rect :: Float -> Float -> Float -> Float -> Picture
rect startX startY endX endY = Color white $ Line [(startX, startY), (endX, startY), (endX, endY), (startX, endY), (startX, startY)]

cell :: Float -> Float -> Float -> Float -> Picture
cell pl pt x y = translate (fromIntegral screenWidth * (-0.5)) (fromIntegral screenHeight * 0) frame
           where
                frame = rect (pl + cellSize * x) (pt + cellSize * y) (pl + cellSize * (x + 1)) (pt + cellSize * (y + 1))

grid :: Int -> Int -> Int -> Int -> [Picture]
grid x y width height = [cell (fromIntegral x) (fromIntegral (-y)) (fromIntegral a) (fromIntegral b) | a <- [0..width - 1], b <- [0..height - 1]]