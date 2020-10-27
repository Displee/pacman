module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Maybe (fromJust, isJust)

-- | Handle the game loop
loop :: Float -> GameState -> IO GameState
loop seconds gstate = return (direction' gstate)

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate@(GameState m s (Player px py pt d nd v sc li) g) = return (inputKey e gstate)

direction' :: GameState -> GameState
direction' (GameState m s (Player px py (Tile x y k ) North nd v sc li) g ) = (GameState m s (Player px (py + 1) (Tile x y k) dir nextDir v sc li) g )
                                                                              where
                                                                                    tileX = screenXToTile(px)
                                                                                    tileY = screenYToTile(py)
                                                                                    realScreenX = tileToScreenX(tileX)
                                                                                    realScreenY = tileToScreenY(tileY)
                                                                                    updateDirection = px == realScreenX || py == realScreenY
                                                                                    dir | updateDirection && isJust nd = fromJust nd
                                                                                        | otherwise = North
                                                                                    nextDir | updateDirection = Nothing
                                                                                            | otherwise = nd
direction' (GameState m s (Player px py (Tile x y k ) South nd v sc li) g) = (GameState m s (Player px (py - 1) (Tile x y k) dir nextDir v sc li) g )
                                                                              where
                                                                                    tileX = screenXToTile(px)
                                                                                    tileY = screenYToTile(py)
                                                                                    realScreenX = tileToScreenX(tileX)
                                                                                    realScreenY = tileToScreenY(tileY)
                                                                                    updateDirection = px == realScreenX || py == realScreenY
                                                                                    dir | updateDirection && isJust nd = fromJust nd
                                                                                        | otherwise = South
                                                                                    nextDir | updateDirection = Nothing
                                                                                            | otherwise = nd
direction' (GameState m s (Player px py (Tile x y k ) East nd v sc li) g) = (GameState m s (Player (px - 1) py (Tile x y k) dir nextDir v sc li) g )
                                                                              where
                                                                                    tileX = screenXToTile(px)
                                                                                    tileY = screenYToTile(py)
                                                                                    realScreenX = tileToScreenX(tileX)
                                                                                    realScreenY = tileToScreenY(tileY)
                                                                                    updateDirection = px == realScreenX || py == realScreenY
                                                                                    dir | updateDirection && isJust nd = fromJust nd
                                                                                        | otherwise = East
                                                                                    nextDir | updateDirection = Nothing
                                                                                            | otherwise = nd
direction' (GameState m s (Player px py (Tile x y k ) West nd v sc li) g) = (GameState m s (Player (px + 1) py (Tile x y k) dir nextDir v sc li) g )
                                                                              where
                                                                                    tileX = screenXToTile(px)
                                                                                    tileY = screenYToTile(py)
                                                                                    realScreenX = tileToScreenX(tileX)
                                                                                    realScreenY = tileToScreenY(tileY)
                                                                                    updateDirection = px == realScreenX || py == realScreenY
                                                                                    dir | updateDirection && isJust nd = fromJust nd
                                                                                        | otherwise = West
                                                                                    nextDir | updateDirection = Nothing
                                                                                            | otherwise = nd

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyUp) _ _ _) (GameState m s (Player px py pp d nd v sc li) g ) = (GameState m s (Player px py pp d (Just North) v sc li) g )
inputKey (EventKey (SpecialKey KeyDown) _ _ _) (GameState m s (Player px py pp d nd v sc li) g ) = (GameState m s (Player px py pp d (Just South) v sc li) g )
inputKey (EventKey (SpecialKey KeyRight) _ _ _) (GameState m s (Player px py pp d nd v sc li) g ) = (GameState m s (Player px py pp d (Just West) v sc li) g )
inputKey (EventKey (SpecialKey KeyLeft) _ _ _) (GameState m s (Player px py pp d nd v sc li) g ) = (GameState m s (Player px py pp d (Just East) v sc li) g )
inputKey _ gstate = gstate
-- | Grid functions

rect :: Float -> Float -> Float -> Float -> Picture
rect startX startY endX endY = Color white $ Line [(startX, startY), (endX, startY), (endX, endY), (startX, endY), (startX, startY)]

cell :: Float -> Float -> Float -> Float -> Picture
cell pl pt x y = rect (pl + cellSize * x) (pt + cellSize * y) (pl + cellSize * (x + 1)) (pt + cellSize * (y + 1))

grid :: Int -> Int -> Int -> Int -> [Picture]
grid x y width height = [cell (fromIntegral x + screenOffsetX) (fromIntegral (-y) + screenOffsetY) (fromIntegral a) (fromIntegral b) | a <- [0..width - 1], b <- [0..height - 1]]
                        where
                             halfScreenHeight :: Int
                             halfScreenHeight = screenHeight `div` 2
                             screenOffsetX :: Float
                             screenOffsetX = fromIntegral screenWidth * (-0.5)
                             screenOffsetY :: Float
                             screenOffsetY = fromIntegral (halfScreenHeight - (height * round cellSize))

fillRect :: Float -> Float -> Float -> Float -> Color -> Picture
fillRect startX startY endX endY c = Color c $ Polygon [(startX, startY), (endX, startY), (endX, endY), (startX, endY), (startX, startY)]

fillCell :: Float -> Float -> Float -> Float -> Color -> Picture
fillCell pl pt x y = fillRect (pl + cellSize * x) (pt + cellSize * y) (pl + cellSize * (x + 1)) (pt + cellSize * (y + 1))

fillCellSmart :: Int -> Int -> Int -> Int -> Color -> Picture
fillCellSmart pl pt x y = fillCell (fromIntegral pl + screenOffsetX) (fromIntegral (-pt) + screenOffsetY) (fromIntegral x) (fromIntegral y)
                          where
                                halfScreenHeight :: Int
                                halfScreenHeight = screenHeight `div` 2
                                screenOffsetX :: Float
                                screenOffsetX = fromIntegral screenWidth * (-0.5)
                                screenOffsetY :: Float
                                screenOffsetY = (fromIntegral halfScreenHeight - cellSize) - ((cellSize * 2) * fromIntegral y)

gridMaker :: Int -> Int ->  [Char]-> [ Tile]
gridMaker x y [] =[]
gridMaker x y (n:ns) | n == '\n' = gridMaker 1 (y+1) ns
                     | otherwise = gridMaker' x y n  : gridMaker (x+1) y ns

gridMaker' :: Int -> Int -> Char -> Tile
gridMaker' x y '.'  = Tile {x= x ,y= y, tileType= Dot}
gridMaker' x y '#'  = Tile {x= x ,y= y, tileType= Wall}
gridMaker' x y '*'  = Tile {x= x ,y= y, tileType= FlashingDot}
gridMaker' x y _  = Tile {x= x ,y= y, tileType= NormalTile}

-- I'm not sure where the 7 and 7.5 come from, I think it has to do with the bmp dimensions of pacman which is 12x13
-- However these two values are used to show pacman in the middle of the tile

tileToScreenX :: Int -> Float
tileToScreenX x = fromIntegral gridPaddingLeft + screenOffsetX + (cellSize * fromIntegral x) + 7
            where
                  screenOffsetX :: Float
                  screenOffsetX = fromIntegral screenWidth * (-0.5)

tileToScreenY :: Int -> Float
tileToScreenY y = fromIntegral (-gridPaddingTop) + screenOffsetY + (cellSize * fromIntegral y) + 7.5
            where
                  halfScreenHeight :: Int
                  halfScreenHeight = screenHeight `div` 2
                  screenOffsetY :: Float
                  screenOffsetY = (fromIntegral halfScreenHeight - cellSize) - ((cellSize * 2) * fromIntegral y)

screenXToTile :: Float -> Int
screenXToTile x = round $ (x - 7 - fromIntegral gridPaddingLeft - screenOffsetX) / cellSize
                  where
                        screenOffsetX :: Float
                        screenOffsetX = fromIntegral screenWidth * (-0.5)

screenYToTile :: Float -> Int
screenYToTile y = -(round $ (y - 7.5 + fromIntegral gridPaddingTop - screenOffsetY) / cellSize)
                  where
                        halfScreenHeight :: Int
                        halfScreenHeight = screenHeight `div` 2
                        screenOffsetY :: Float
                        screenOffsetY = fromIntegral halfScreenHeight - cellSize