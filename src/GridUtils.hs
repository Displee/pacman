module GridUtils where

import Model
import Graphics.Gloss

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

drawTileGrid :: GameState -> Picture
drawTileGrid (GameState (Maze w h l t) status p _ _ _ _) = Pictures $ grid gridPaddingLeft gridPaddingTop w h ++ filled t
                                                             where
                                                                  fillTile :: Tile -> Picture
                                                                  fillTile (Tile x y Dot) = Pictures [fillCellSmart gridPaddingLeft gridPaddingTop x y black, translate (tileToScreenX x) (tileToScreenY y) (Color yellow $ Graphics.Gloss.circleSolid 3)]
                                                                  fillTile (Tile x y FlashingDot) = Pictures [fillCellSmart gridPaddingLeft gridPaddingTop x y black, translate (tileToScreenX x) (tileToScreenY y) (Color white $ Graphics.Gloss.circleSolid 6)]
                                                                  fillTile (Tile x y NormalTile) = fillCellSmart gridPaddingLeft gridPaddingTop x y black
                                                                  fillTile (Tile x y Wall) = fillCellSmart gridPaddingLeft gridPaddingTop x y blue
                                                                  fillTile (Tile x y JailDoor) = fillCellSmart gridPaddingLeft gridPaddingTop x y orange
                                                                  filled xs = map fillTile xs
                                                                  
gridMaker :: Int -> Int ->  [Char]-> [ Tile]
gridMaker x y [] =[]
gridMaker x y (n:ns) | n == '\n' = gridMaker 1 (y+1) ns
                     | otherwise = gridMaker' x y n  : gridMaker (x+1) y ns

gridMaker' :: Int -> Int -> Char -> Tile
gridMaker' x y '.'  = Tile {x= x ,y= y, tileType= Dot}
gridMaker' x y '#'  = Tile {x= x ,y= y, tileType= Wall}
gridMaker' x y '-'  = Tile {x= x ,y= y, tileType= JailDoor}
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
