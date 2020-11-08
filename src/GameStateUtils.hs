module GameStateUtils where

import Model

import Graphics.Gloss (loadBMP)
import GridUtils (tileToScreenX, tileToScreenY)

readPlayer :: Int -> Int -> [Char]-> Tile
readPlayer _ _ [] = Tile 1 1 NormalTile
readPlayer tx ty (n:ns) | n == '\n' = readPlayer 1 (ty+1) ns
                        | n == 'O' = Tile tx ty NormalTile
                        | otherwise = readPlayer (tx + 1) ty ns

createPlayer :: p -> Int -> Int -> IO Player
createPlayer m tx ty = do
                          icon <- loadBMP "data/ghost/pacmanright1.bmp"
                          icons  <- pacmanviewer
                          return $ Player icon icons spawnTile (tileToScreenX tx) (tileToScreenY ty) spawnTile West Nothing 1 0 3 0
                                 where
                                       spawnTile = Tile tx ty NormalTile

readGhost :: Int -> Int -> Char -> [Char] -> IO (Maybe Ghost)
readGhost _ _ _ [] = return Nothing
readGhost tx ty c (n:ns) | n == '\n' = readGhost 1 (ty + 1) c ns
                         | n == c = createGhost c tx ty
                         | otherwise = readGhost (tx + 1) ty c ns

createGhost :: Char -> Int -> Int -> IO (Maybe Ghost)
createGhost 'P' tx ty = do
                        pinkyg <- loadBMP "data/PinkGhost/GhostPinkWest1.bmp"
                        pinkghosticons <- pinkGhostviewer
                        return $ Just $ Ghost (tileToScreenX tx) (tileToScreenY ty) pinkyg pinkghosticons spawnTile Pinky spawnTile West West Nothing 1 Chase 1 1 startGhostCageTicks startGhostCageTicks
                               where
                                     spawnTile = Tile tx ty NormalTile
createGhost 'I' tx ty = do
                        inkyg <- loadBMP "data/ghost/pinky.bmp"
                        blueghosticons <- blueGhostviewer
                        return $ Just $ Ghost (tileToScreenX tx) (tileToScreenY ty) inkyg blueghosticons spawnTile Inky spawnTile West West Nothing 1 Chase 1 1 startGhostCageTicks startGhostCageTicks
                               where
                                     spawnTile = Tile tx ty NormalTile
createGhost 'B' tx ty = do
                        blinkyg <- loadBMP "data/ghost/ghostred.bmp"
                        redghosticons <- redGhostviewer
                        return $ Just $ Ghost (tileToScreenX tx) (tileToScreenY ty) blinkyg redghosticons spawnTile Blinky spawnTile West West Nothing 1 Chase 1 1 startGhostCageTicks startGhostCageTicks
                               where
                                     spawnTile = Tile tx ty NormalTile
createGhost 'C' tx ty = do
                        clydeg <- loadBMP "data/ghost/ghostblue.bmp"
                        yellowghosticons <- yellowGhostviewer
                        return $ Just $ Ghost (tileToScreenX tx) (tileToScreenY ty) clydeg yellowghosticons spawnTile Clyde spawnTile West West Nothing 1 Chase 1 1 startGhostCageTicks startGhostCageTicks
                               where
                                     spawnTile = Tile tx ty NormalTile
createGhost _ tx ty = return Nothing

scattermode :: IO [Animation]
scattermode = do
          scatterblue1 <- loadBMP "data/ScatterGhost/ScatterBlue1.bmp"
          scatterblue2 <- loadBMP "data/ScatterGhost/ScatterBlue2.bmp"
          scatterwhite1 <- loadBMP "data/ScatterGhost/ScatterWhite1.bmp"
          scatterwhite2 <- loadBMP "data/ScatterGhost/ScatterWhite2.bmp"
          return ((Scattermode Blue 1 scatterblue1) :(Scattermode Blue 2 scatterblue2) :(Scattermode White 1 scatterwhite1):(Scattermode White 2 scatterwhite2):[])

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

findJailPositions :: Int -> Int -> (Tile, Tile) -> [Char] -> (Tile, Tile)
findJailPositions _ _ _ [] = (Tile 0 0 NormalTile, Tile 0 0 NormalTile)
findJailPositions tx ty (accX, accY) (n:ns) | n == '\n' = findJailPositions 1 (ty + 1) (accX, accY) ns
                                            | n == 'X' = findJailPositions (tx + 1) ty (Tile tx ty NormalTile, accY) ns
                                            | n == 'Y' = (accX, Tile tx ty NormalTile)
                                            | otherwise = findJailPositions (tx + 1) ty (accX, accY) ns