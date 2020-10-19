module Model where

data GameStatus = NotPlaying | Playing | GameOver

data GameState = GameState {
    maze :: Maze,
    status :: GameStatus,
    player :: Player
}

data TileType = Dot | FlashingDot | NormalTile | Wall
                deriving(Eq, Show)

data Tile = Tile {
    x :: Int,
    y :: Int,
    tileType :: TileType
} deriving(Show)

data Maze = Maze {
    width :: Int,
    height :: Int,
    level :: Int,
    tiles :: [Tile]
}

data Player = Player {
    position :: Tile
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
