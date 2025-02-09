module Model where

import Graphics.Gloss.Data.Picture (Picture)

startTimeGameTicks :: Int
startTimeGameTicks = 60 * 4

respawnTick :: Int
respawnTick = 60 * 4

nextLevelTick :: Int
nextLevelTick = 60 * 4

restartLevelTicks :: Int
restartLevelTicks = 60 * 4

data GameStatus = Starting | Playing | Paused | GameOver deriving (Eq, Show)

data Animation = Animation Direction Int Picture | Solid Picture  | Scattermode GhostColor Int Picture  deriving Eq

data GhostColor = White | Blue deriving Eq

data GameState = GameState {
      maze :: Maze,
      status :: GameStatus,
      player :: Player,
      ghosts :: [Ghost],
      gameTick :: Int,
      jailTiles :: (Tile, Tile),
      winTick :: Int
  }

data TileType = Dot | FlashingDot | NormalTile | Wall | JailDoor deriving(Eq, Show)
data Direction = North | East | South | West deriving(Eq, Show)

data GhostType = Pinky | Inky| Blinky | Clyde deriving Eq

data Mode = Scatter | Chase | Frighten | Frightenwhite deriving(Eq, Show)

data Ghost = Ghost {
                      gx :: Float,
                      gy :: Float,
                      ghostIcon :: Picture,
                      ghostIcons :: [Animation],
                      ghostSpawnTile :: Tile,
                      ghosttype :: GhostType,
                      posghost :: Tile,
                      prevloc :: Direction,
                      directionghost :: Direction,
                      nextGhostDirection :: Maybe Direction,
                      velocityghost:: Int,
                      mode :: Mode ,
                      targetLocationx:: Int,
                      targetLocationy :: Int,
                      cageTicks :: Int,
                      frightenTicks::Int
                      }
data Tile = Tile {
      x :: Int,
      y :: Int,
      tileType :: TileType
                    } deriving(Show, Eq)


data Maze = Maze {
      width  :: Int,
      height :: Int,
      level  :: Int,
      tiles  :: [Tile]
                  }

data Player = Player {
                      playerIcon :: Picture,
                      playerIcons :: [Animation],
                      playerSpawnTile :: Tile,
                      playerX :: Float,
                      playerY :: Float,
                      posplayer :: Tile,
                      direction :: Direction,
                      nextPlayerDirection :: Maybe Direction,
                      velocity :: Int,
                      score :: Int,
                      lifes :: Int,
                      deadTick :: Int
                      }


screenWidth :: Int
screenWidth = 800

screenHeight :: Int
screenHeight = 600

cellSize :: Float
cellSize = 15

gridPaddingLeft :: Int
gridPaddingLeft = 10

gridPaddingTop :: Int
gridPaddingTop = 10

startGhostCageTicks :: Int
startGhostCageTicks = 50
