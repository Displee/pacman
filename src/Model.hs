module Model where

data GameStatus = NotPlaying | Playing | GameOver

data GameState = GameState {
    maze :: Maze,
    status :: GameStatus
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

screenWidth :: Int
screenWidth = 800

screenHeight :: Int
screenHeight = 600

cellSize :: Float
cellSize = 15

initialState :: GameState
initialState = GameState (Maze 300 300 0) NotPlaying