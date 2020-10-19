module Model where

data GameStatus = NotPlaying | Playing | GameOver

data GameState = GameState {
    maze :: Maze,
    status :: GameStatus,
    player :: Player
}

data Tile = Tile {
    x :: Int,
    y :: Int
}

data Maze = Maze {
    width :: Int,
    height :: Int,
    level :: Int
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

initialState :: GameState
initialState = GameState (Maze 50 15 0) NotPlaying (Player (Tile 3 3))