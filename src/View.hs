module View where

import Model
import Controller

import Graphics.Gloss

getGhostIcon  :: Ghost ->  Picture
getGhostIcon (Ghost gx gy gi _ _ _ _ _ _  _ _ _ _ _)   = translate gx gy gi

getGhostIcon' :: Ghost -> Picture -> Picture
getGhostIcon' (Ghost gx gy gi _ _ _ _ _ _  _ _ _ _ _) pic  = translate gx gy pic
view :: GameState -> IO Picture
view gs@(GameState _ _ (Player pi pis px py (Tile x y _) d _ _ _ _ ) g gt) = do
                                let gridPicture = viewPure gs
                                let pacman = translate px py (picturechanger pis d gt)
                                let pinkghostpic = ghostchanger (takeghost Pinky g) gt
                                let blueghostpic = ghostchanger (takeghost Inky g) gt
                                let yellowghostpic = ghostchanger(takeghost Clyde g) gt
                                let redghostpic = ghostchanger(takeghost Blinky g) gt
                                let pinky = getGhostIcon' (takeghost Pinky g) pinkghostpic
                                let inky = getGhostIcon' (takeghost Inky g) blueghostpic
                                let blinky = getGhostIcon' (takeghost Blinky g) redghostpic
                                let clyde = getGhostIcon' (takeghost Clyde g) yellowghostpic
                                let scorePicture = translate 0 0 $ scale (-100) (-100) $ color white $ text ("Score: 0" ++ show 0)
                                --let lifesPicture = translate 0 0 $ color white $ text ("Lifes: " ++ show 0)
                                return (Pictures [gridPicture, pacman,pinky, inky, blinky, clyde, scorePicture])


drawTileGrid :: GameState -> Picture
drawTileGrid (GameState (Maze w h l t) status p _ _) = Pictures $ (grid gridPaddingLeft gridPaddingTop w h) ++ filled t
                                                             where
                                                                  tileColor t | t == NormalTile = black
                                                                              | t == Wall = blue
                                                                              | t == Dot = red
                                                                              | t == FlashingDot = green
                                                                  fillTile (Tile x y t) | t == Dot = Pictures [fillCellSmart gridPaddingLeft gridPaddingTop x y (black), translate (tileToScreenX x) (tileToScreenY y) (Color yellow $ Graphics.Gloss.circleSolid 3)]
                                                                                        | t == FlashingDot = Pictures [fillCellSmart gridPaddingLeft gridPaddingTop x y (black), translate (tileToScreenX x) (tileToScreenY y) (Color white $ Graphics.Gloss.circleSolid 6)]
                                                                                        | otherwise = fillCellSmart gridPaddingLeft gridPaddingTop x y (tileColor t)
                                                                  filled xs = map fillTile xs

viewPure :: GameState -> Picture
viewPure gs = case status gs of
  NotPlaying  -> drawTileGrid gs
  Playing     -> color green (text "Playing!")
  GameOver    -> color green (text "Game over!")

pacmanviewer :: IO [Animation]
pacmanviewer = do 
          pacNorth1 <- loadBMP "data/Pacman/pac-man-up-1.bmp"
          pacNorth2 <- loadBMP "data/Pacman/pac-man-up-2.bmp"
          pacSouth1 <- loadBMP "data/Pacman/pac-man-down1.bmp"
          pacSouth2 <- loadBMP "data/Pacman/pac-man-down2.bmp"
          pacWest1  <- loadBMP "data/Pacman/pac-man-left1.bmp"
          pacWest2  <- loadBMP "data/Pacman/pac-man-left2.bmp"
          pacEast1  <- loadBMP "data/Pacman/pac-man-right1.bmp"
          pacEast2  <- loadBMP "data/Pacman/pac-man-right2.bmp"
          solid     <- loadBMP "data/Pacman/solid.bmp"

          return((Animation North 1 pacNorth1):(Animation North 2 pacNorth2):(Animation South 1 pacSouth1):
               (Animation South 2 pacSouth2): (Animation West 1 pacWest1):(Animation West 2 pacWest2):
               (Animation East 1 pacEast1) :(Animation East 2 pacEast2) :(Solid solid) : []) 

picturechanger icons d  gt = case (gt `div` 3) `mod` 3 of
                                        0 -> takepic d 1 icons
                                        1 -> takepic d 2 icons
                                        _ -> takesolid icons
ghostchanger :: Integral a => Ghost -> a -> Picture
ghostchanger (Ghost gx gy gi gis _ _ _ d _  _ Scatter _ _ _)  gt = case (gt `div` 3) `mod` 2 of
                                                                 0 -> takescatter gis 1 Blue
                                                                 _ -> takescatter gis 2 Blue
                                                                 
ghostchanger (Ghost gx gy gi gis _ _ _ d _  _ _ _ _ _)  gt =case (gt `div` 3) `mod` 2 of
                                                                 0 -> takepic d 1 gis
                                                                 _ -> takepic d 2 gis
                                       
                                       
takeghost :: GhostType -> [Ghost] -> Ghost
takeghost ghost (g@(Ghost _ _ _ _ name _ _ _ _ _ _ _ _ _):gs) | name == ghost = g
                                                              |otherwise      =  takeghost ghost gs

takesolid :: [Animation] -> Picture
takesolid ((Solid k):xs)= k
takesolid (x:xs) = takesolid xs 

takescatter :: [Animation]->Int -> GhostColor -> Picture
takescatter ((Scattermode c i pic ):xs) num col | c == col && i == num = pic 
                                                |otherwise = takescatter xs num col

takepic :: Direction -> Int -> [Animation] -> Picture
takepic d i ((Animation di ti p ):xs) | di == d && ti == i = p 
                                      |otherwise = takepic d i xs 
pinkGhostviewer :: IO [Animation]
pinkGhostviewer = do 
          pinkNorth1 <- loadBMP "data/PinkGhost/GhostPinkNorth1.bmp"
          pinkNorth2 <- loadBMP "data/PinkGhost/GhostPinkNorth2.bmp"
          pinkSouth1 <- loadBMP "data/PinkGhost/GhostPinkSouth1.bmp"
          pinkSouth2 <- loadBMP "data/PinkGhost/GhostPinkSouth2.bmp"
          pinkWest1  <- loadBMP "data/PinkGhost/GhostPinkWest1.bmp"
          pinkWest2  <- loadBMP "data/PinkGhost/GhostPinkWest2.bmp"
          pinkEast1  <- loadBMP "data/PinkGhost/GhostPinkEast1.bmp"
          pinkEast2  <- loadBMP "data/PinkGhost/GhostPinkEast2.bmp"
          scatterghosts <- scattermode

          return(((Animation North 1 pinkNorth1):(Animation North 2 pinkNorth2):(Animation South 1 pinkSouth1):
               (Animation South 2 pinkSouth2): (Animation West 1 pinkWest1):(Animation West 2 pinkWest2):
               (Animation East 1 pinkEast1) :(Animation East 2 pinkEast2)  : []) ++ scatterghosts) 

blueGhostviewer :: IO [Animation]
blueGhostviewer = do 
          blueNorth1 <- loadBMP "data/GhostBlue/BlueGhostNorth1.bmp"
          blueNorth2 <- loadBMP "data/GhostBlue/BlueGhostNorth2.bmp"
          blueSouth1 <- loadBMP "data/GhostBlue/BlueGhostSouth1.bmp"
          blueSouth2 <- loadBMP "data/GhostBlue/BlueGhostSouth2.bmp"
          blueWest1  <- loadBMP "data/GhostBlue/BlueGhostWest1.bmp"
          blueWest2  <- loadBMP "data/GhostBlue/BlueGhostWest2.bmp"
          blueEast1  <- loadBMP "data/GhostBlue/BlueGhostEast1.bmp"
          blueEast2  <- loadBMP "data/GhostBlue/BlueGhostEast2.bmp"
          scatterghosts <- scattermode


          return(((Animation North 1 blueNorth1):(Animation North 2 blueNorth2):(Animation South 1 blueSouth1):
               (Animation South 2 blueSouth2): (Animation West 1 blueWest1):(Animation West 2 blueWest2):
               (Animation East 1 blueEast1) :(Animation East 2 blueEast2)  : [])++ scatterghosts) 


yellowGhostviewer :: IO [Animation]
yellowGhostviewer = do 
          yellowNorth1 <- loadBMP "data/GhostYellow/YellowGhostNorth1.bmp"
          yellowNorth2 <- loadBMP "data/GhostYellow/YellowGhostNorth2.bmp"
          yellowSouth1 <- loadBMP "data/GhostYellow/YellowGhostSouth1.bmp"
          yellowSouth2 <- loadBMP "data/GhostYellow/YellowGhostSouth2.bmp"
          yellowWest1  <- loadBMP "data/GhostYellow/YellowGhostWest1.bmp"
          yellowWest2  <- loadBMP "data/GhostYellow/YellowGhostWest2.bmp"
          yellowEast1  <- loadBMP "data/GhostYellow/YellowGhostEast1.bmp"
          yellowEast2  <- loadBMP "data/GhostYellow/YellowGhostEast2.bmp"
          scatterghosts <- scattermode


          return(((Animation North 1 yellowNorth1):(Animation North 2 yellowNorth2):(Animation South 1 yellowSouth1):
               (Animation South 2 yellowSouth2): (Animation West 1 yellowWest1):(Animation West 2 yellowWest2):
               (Animation East 1 yellowEast1) :(Animation East 2 yellowEast2)  :[])++scatterghosts) 

redGhostviewer :: IO [Animation]
redGhostviewer = do 
          redNorth1 <- loadBMP "data/GhostRed/RedGhostNorth1.bmp"
          redNorth2 <- loadBMP "data/GhostRed/RedGhostNorth2.bmp"
          redSouth1 <- loadBMP "data/GhostRed/RedGhostSouth1.bmp"
          redSouth2 <- loadBMP "data/GhostRed/RedGhostSouth2.bmp"
          redWest1  <- loadBMP "data/GhostRed/RedGhostWest1.bmp"
          redWest2  <- loadBMP "data/GhostRed/RedGhostWest2.bmp"
          redEast1  <- loadBMP "data/GhostRed/RedGhostEast1.bmp"
          redEast2  <- loadBMP "data/GhostRed/RedGhostEast2.bmp"
          scatterghosts <- scattermode
    

          return(((Animation North 1 redNorth1):(Animation North 2 redNorth2):(Animation South 1 redSouth1):
               (Animation South 2 redSouth2): (Animation West 1 redWest1):(Animation West 2 redWest2):
               (Animation East 1 redEast1) :(Animation East 2 redEast2)  : [])++ scatterghosts) 

scattermode :: IO [Animation]
scattermode = do
          scatterblue1 <- loadBMP "data/ScatterGhost/ScatterBlue1.bmp"
          scatterblue2 <- loadBMP "data/ScatterGhost/ScatterBlue2.bmp"
          scatterwhite1 <- loadBMP "data/ScatterGhost/ScatterWhite1.bmp"
          scatterwhite2 <- loadBMP "data/ScatterGhost/ScatterWhite2.bmp"
          return ((Scattermode Blue 1 scatterblue1) :(Scattermode Blue 2 scatterblue2) :(Scattermode White 1 scatterwhite1):(Scattermode White 2 scatterwhite2):[])