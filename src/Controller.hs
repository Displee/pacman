module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Maybe (fromJust, isJust)

-- | Handle the game loop
loop :: Float -> GameState -> IO GameState
loop seconds gstate@(GameState m s p g gt) = do
                                             let player = handlePlayerMovement m p
                                             let ghosts = map (handleGhostMovement m) g
                                             return (GameState m s player ghosts (gt + 1))

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate@(GameState m s (Player _ px py pt d nd v sc li) g gt) = return (inputKey e gstate)

handlePlayerMovement :: Maze -> Player -> Player
handlePlayerMovement m (Player pic px py (Tile _ _ tt) d nd v sc li) = Player pic newPx newPy (Tile tileX tileY tt) dir nextDir v sc li
                                                                       where
                                                                             tileX = screenXToTile px
                                                                             tileY = screenYToTile py
                                                                             onTile = isOnTile px py tileX tileY
                                                                             tileInFrontIsWall = tileInFrontIs m tileX tileY d Wall
                                                                             dir | onTile && isJust nd && not (tileInFrontIs m tileX tileY (fromJust nd) Wall) = fromJust nd
                                                                                 | otherwise = d
                                                                             sdv = screenDirectionVector dir
                                                                             newPx | tileInFrontIsWall && onTile = px
                                                                                   | otherwise = px + fromIntegral(fst sdv)
                                                                             newPy | tileInFrontIsWall && onTile = py
                                                                                   | otherwise = py + fromIntegral(snd sdv)
                                                                             nextDir | onTile && isJust nd && not (tileInFrontIs m tileX tileY (fromJust nd) Wall) = Nothing
                                                                                     | otherwise = nd

handleGhostMovement :: Maze -> Ghost -> Ghost
handleGhostMovement m (Ghost gx gy gi gt (Tile _ _ tt) d nd v mo tl gct) = Ghost newGx newWGy gi gt (Tile tileX tileY tt) dir nextDir v mo tl gct
                                                        where
                                                             tileX = screenXToTile gx
                                                             tileY = screenYToTile gy
                                                             onTile = isOnTile gx gy tileX tileY
                                                             tileInFrontIsWall = tileInFrontIs m tileX tileY d Wall
                                                             dir | onTile && isJust nd && not (tileInFrontIs m tileX tileY (fromJust nd) Wall) = fromJust nd
                                                                 | otherwise = d
                                                             sdv = screenDirectionVector dir
                                                             newGx | tileInFrontIsWall && onTile = gx
                                                                   | otherwise = gx + fromIntegral(fst sdv)
                                                             newWGy | tileInFrontIsWall && onTile = gy
                                                                    | otherwise = gy + fromIntegral(snd sdv)
                                                             nextDir | onTile && isJust nd && not (tileInFrontIs m tileX tileY (fromJust nd) Wall) = Nothing
                                                                     | otherwise = nd

tileInFront :: Maze -> Direction -> Int -> Int -> Tile
tileInFront (Maze _ _ _ xs) d cx cy = head $ filter (\(Tile tx ty _) -> tx == (cx + fst v) && ty == (cy + snd v)) xs
                                      where
                                            v = tileDirectionVector d

getTileType :: Tile -> TileType
getTileType (Tile _ _ tt) = tt

tileInFrontIs :: Maze -> Int -> Int -> Direction -> TileType -> Bool
tileInFrontIs m tx ty d tt = getTileType (tileInFront m d tx ty) == tt

isOnTile :: Float -> Float -> Int -> Int -> Bool
isOnTile sx sy tx ty = sx == tileToScreenX tx && sy == tileToScreenY ty

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyUp) _ _ _) (GameState m s (Player pi px py pp d nd v sc li) g gt) = GameState m s (Player pi px py pp d (Just North) v sc li) g gt
inputKey (EventKey (SpecialKey KeyDown) _ _ _) (GameState m s (Player pi px py pp d nd v sc li) g gt) = GameState m s (Player pi px py pp d (Just South) v sc li) g gt
inputKey (EventKey (SpecialKey KeyRight) _ _ _) (GameState m s (Player pi px py pp d nd v sc li) g gt) = GameState m s (Player pi px py pp d (Just East) v sc li) g gt
inputKey (EventKey (SpecialKey KeyLeft) _ _ _) (GameState m s (Player pi px py pp d nd v sc li) g gt) = GameState m s (Player pi px py pp d (Just West) v sc li) g gt
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

tileDirectionVector :: Direction -> (Int, Int)
tileDirectionVector North = (0, -1)
tileDirectionVector South = (0, 1)
tileDirectionVector East = (1, 0)
tileDirectionVector West = (-1, 0)

screenDirectionVector :: Direction -> (Int, Int)
screenDirectionVector North = (0, 1)
screenDirectionVector South = (0, -1)
screenDirectionVector East = (1, 0)
screenDirectionVector West = (-1, 0)
