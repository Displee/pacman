module Model where

data GameStatus = NotPlaying | Playing | GameOver deriving (Eq, Show)

data GameState = GameState {
      maze :: Maze,
      status :: GameStatus,
      player :: Player,
      ghosts :: [Ghost]
  }

data TileType = Dot | FlashingDot | NormalTile | Wall
                  deriving(Eq, Show)
data Direction = North | East | South | West deriving(Eq, Show)

data GhostType = Pinky | Inky| Blinky | Clyde

data Mode = Scatter | Chase | Frighten

data Ghost = Ghost {
                      ghosttype :: GhostType,
                      posghost :: Tile,
                      directionghost :: Direction,
                      velocityghost:: Int,
                      mode :: Mode ,
                      targetLocation:: Tile
                      }
data Tile = Tile {
      x :: Int,
      y :: Int,
      tileType :: TileType
                    } deriving(Show)


data Maze = Maze {
      width  :: Int,
      height :: Int,
      level  :: Int,
      tiles  :: [Tile]                  
                  }

data Player = Player {
                      playerX :: Float,
                      playerY :: Float,
                      posplayer :: Tile,
                      direction :: Direction,
                      nextDirection :: Maybe Direction,
                      velocity :: Int,
                      score :: Int,
                      lifes :: Int
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
