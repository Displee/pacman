module View where

import Model
import Controller

import Graphics.Gloss

getGhostIcon :: Ghost -> Picture
getGhostIcon (Ghost gx gy gi _ _ _ _ _  _ _ _ _ _) = translate gx gy gi

view :: GameState -> IO Picture
view gs@(GameState _ _ (Player pi px py (Tile x y _) _ _ _ _ _ ) g _) = do
                                let gridPicture = drawTileGrid gs
                                let pacman = translate px py pi
                                let ghost1 = getGhostIcon $ head g
                                let ghost2 = getGhostIcon $ g!!1
                                let ghost3 = getGhostIcon $ g!!2
                                let ghost4 = getGhostIcon $ g!!3
                                let scorePicture = translate 0 0 $ scale (-100) (-100) $ color white $ text ("Score: 0" ++ show 0)
                                --let lifesPicture = translate 0 0 $ color white $ text ("Lifes: " ++ show 0)
                                return (Pictures [gridPicture, pacman, ghost1, ghost2, ghost3, ghost4, scorePicture])

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
