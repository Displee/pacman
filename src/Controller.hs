module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Maybe (fromJust, isJust)

-- | Handle the game loop
loop :: Float -> GameState -> IO GameState
loop seconds gstate@(GameState m s (Player pi px py t d nd v sc li) g) = do
                                                                       let ghosts = targetLocation' d t g

                                                                       return (direction' (GameState m s (Player pi px py t d nd v sc li) ghosts))

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate@(GameState m s (Player _ px py pt d nd v sc li) g) = return (inputKey e gstate)

targetLocation' :: Direction-> Tile -> [Ghost] -> [Ghost] 
targetLocation' _ _ []= []
targetLocation' d t@(Tile x y _) (g@(Ghost _ _ _ Inky _ _ _ _ _ _):gs)   = targetInky d x y g : targetLocation' d t gs
targetLocation' d t@(Tile x y _) (g@(Ghost _ _ _ Blinky _ _ _ _ _ _):gs) = targetBlinky d x y g : targetLocation' d t gs
targetLocation' d t@(Tile x y _) (g@(Ghost _ _ _ Clyde _ _ _ _ _ _):gs)  = targetClyde d x y g : targetLocation' d t gs
targetLocation' d t@(Tile x y _) (g@(Ghost _ _ _ _ _ _ _ _ _ _):gs)      = targetPinky d x y g : targetLocation' d t gs

--The target location of Blinky is the location of Pac-man.
targetBlinky :: Direction->Int -> Int -> Ghost -> Ghost
targetBlinky d x y g@(Ghost gx gy gi gt pg dg vg m ttx tty) | m == Chase = Ghost gx gy gi gt pg dg vg m x y 
                                                          | m == Scatter = Ghost gx gy gi gt pg dg vg m 25 2
                                                          | otherwise = g 
--The target location of Pinky is 4 tiles in front of pac-man in the direction it is going
targetPinky :: Direction -> Int -> Int -> Ghost -> Ghost
targetPinky d x y g@(Ghost gx gy gi gt pg dg vg m ttx tty) | m == Chase = case d of 
                                                                              North -> Ghost gx gy gi gt pg dg vg m x (y+4)
                                                                              South -> Ghost gx gy gi gt pg dg vg m x (y-4)
                                                                              West  -> Ghost gx gy gi gt pg dg vg m (x+4) y
                                                                              _     -> Ghost gx gy gi gt pg dg vg m (x-4) y
                                                          | m == Scatter = (Ghost gx gy gi gt pg dg vg m 4 2)
                                                          | otherwise = g 
--When clyde is in a proximity of 8 tiles of Pac-man the target location is pac-man itself. Otherwise,the target location is the scatter location.
targetClyde :: Direction -> Int -> Int -> Ghost -> Ghost
targetClyde _ x y (Ghost gx gy gi gt (Tile tx ty tt) dg vg m ttx tty) | m == Chase  = proxof8tiles                                                                                                                  
                                                                      | m == Scatter = (Ghost gx gy gi gt (Tile tx ty tt) dg vg m 7 24)
                                                                      | otherwise    = (Ghost gx gy gi gt (Tile tx ty tt) dg vg m ttx tty) where 
                    proxof8tiles   | ((((x+y) >= (tx+ty)) && (((x+y) - (tx+ty)) <= 8)) ||  (((tx+ty) >= (x+y)) && (((tx+ty) - (x+y)) <= 8))) = (Ghost gx gy gi gt (Tile tx ty tt) dg vg m ttx tty)    
                                   | otherwise  =(Ghost gx gy gi gt (Tile tx ty tt) dg vg m 7 24)

--The target location of Inky is 2 tiles in the direction pac-man is going.
targetInky :: Direction -> Int -> Int -> Ghost -> Ghost
targetInky d x y g@(Ghost gx gy gi gt pg dg vg m ttx tty)  | m == Chase = case d of 
                                                                              North -> Ghost gx gy gi gt pg dg vg m x (y+2)
                                                                              South -> Ghost gx gy gi gt pg dg vg m x (y-2)
                                                                              West  -> Ghost gx gy gi gt pg dg vg m (x+2) y
                                                                              _     -> Ghost gx gy gi gt pg dg vg m (x-2) y
                                                             | m == Scatter = (Ghost gx gy gi gt pg dg vg m 20 24) 
                                                             | otherwise = g 

--possibledirections g@(Ghost gx gy gi gt pg dg vg m ttx tty)  =  


direction' :: GameState -> GameState
direction' (GameState m s p@(Player pi px py (Tile x y tt) d nd v sc li) g ) = GameState m s (Player pi newPx newPy (Tile tileX tileY tt) dir nextDir v sc li) g
                                                                            where
                                                                                  tileX = screenXToTile px
                                                                                  tileY = screenYToTile py
                                                                                  frontTileX | d == West = tileX - 1
                                                                                             | d == East = tileX + 1
                                                                                             | otherwise = tileX
                                                                                  frontTileY | d == North = tileY - 1
                                                                                             | d == South = tileY + 1
                                                                                             | otherwise = tileY
                                                                                  updateDirection = playerIsOnTileInt p tileX tileY
                                                                                  frontTileIsWall = getTileType (tileInFront m frontTileX frontTileY) == Wall
                                                                                  dir | updateDirection && isJust nd = fromJust nd
                                                                                      | otherwise = d
                                                                                  newPx | frontTileIsWall && updateDirection = px
                                                                                        | dir == West = px - 1
                                                                                        | dir == East = px + 1
                                                                                        | otherwise = px
                                                                                  newPy | frontTileIsWall && updateDirection = py
                                                                                        | dir == North = py + 1
                                                                                        | dir == South = py - 1
                                                                                        | otherwise = py
                                                                                  nextDir | updateDirection = Nothing
                                                                                          | otherwise = nd

tileInFront :: Maze -> Int -> Int -> Tile
tileInFront (Maze _ _ _ xs) x y = head $ filter (\(Tile tx ty _) -> tx == x && ty == y) xs

getTileType :: Tile -> TileType
getTileType (Tile _ _ t) = t

playerIsOnTile :: Player -> Tile -> Bool
playerIsOnTile (Player _ px py _ _ _ _ _ _) (Tile x y _) = px == tileToScreenX x && py == tileToScreenY y

playerIsOnTileInt :: Player -> Int -> Int -> Bool
playerIsOnTileInt (Player _ px py _ _ _ _ _ _) x y = px == tileToScreenX x && py == tileToScreenY y

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyUp) _ _ _) (GameState m s (Player pi px py pp d nd v sc li) g ) = GameState m s (Player pi px py pp d (Just North) v sc li) g
inputKey (EventKey (SpecialKey KeyDown) _ _ _) (GameState m s (Player pi px py pp d nd v sc li) g ) = GameState m s (Player pi px py pp d (Just South) v sc li) g
inputKey (EventKey (SpecialKey KeyRight) _ _ _) (GameState m s (Player pi px py pp d nd v sc li) g ) = GameState m s (Player pi px py pp d (Just East) v sc li) g
inputKey (EventKey (SpecialKey KeyLeft) _ _ _) (GameState m s (Player pi px py pp d nd v sc li) g ) = GameState m s (Player pi px py pp d (Just West) v sc li) g
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