module View where

import Model

import Graphics.Gloss
import Data.Fixed (mod')
import GridUtils

getGhostIcon :: Ghost -> Picture -> Picture
getGhostIcon (Ghost gx gy _ _ _ _ _ _ _ _ _ _ _ _ _ _) = translate gx gy

view :: GameState -> IO Picture
view gs@(GameState _ status (Player _ pis _ px py (Tile x y _) d _ _ _ _ _) g gt _ _) = do
                                let gridPicture = drawTileGrid gs
                                let pacman = translate px py (picturechanger pis d status gt (takeghost Pinky g))
                                let pinkghostpic = ghostchanger (takeghost Pinky g) status gt
                                let blueghostpic = ghostchanger (takeghost Inky g) status gt
                                let yellowghostpic = ghostchanger(takeghost Clyde g) status gt
                                let redghostpic = ghostchanger(takeghost Blinky g) status gt
                                let pinky = getGhostIcon (takeghost Pinky g) pinkghostpic
                                let inky = getGhostIcon (takeghost Inky g) blueghostpic
                                let blinky = getGhostIcon (takeghost Blinky g) redghostpic
                                let clyde = getGhostIcon (takeghost Clyde g) yellowghostpic
                                --let scorePicture = translate 0 0 $ scale (-100) (-100) $ color white $ text ("Score: 0" ++ show 0)
                                --let lifesPicture = translate 0 0 $ color white $ text ("Lifes: " ++ show 0)
                                return (Pictures [gridPicture, pacman, pinky, inky, blinky, clyde])

picturechanger :: [Animation] -> Direction -> GameStatus -> Int-> Ghost -> Picture

picturechanger icons d status gt (Ghost _ _ _ _ _ _ _ _ _ _  _ mode _ _ _ ft)   | status == Playing && (mode ==Frighten || mode == Frightenwhite) = case (ft `div` 3) `mod'` 3 of
                                                                                                                                                        0 -> takepic d 1 icons
                                                                                                                                                        1 -> takepic d 2 icons
                                                                                                                                                        _ -> takesolid icons
                                                                                  | status == Playing                                               = case (gt `div` 3) `mod'` 3 of
                                                                                                                                                        0 -> takepic d 1 icons
                                                                                                                                                        1 -> takepic d 2 icons
                                                                                                                                                        _ -> takesolid icons
                                                                                  | otherwise = takesolid icons

ghostchanger :: Ghost -> GameStatus -> Int -> Picture
ghostchanger (Ghost gx gy _ icons _ _ _ _ _ _  _ Frightenwhite _ _ _ ft) status _ | status == Playing = case (ft `div` 6) `mod'` 4 of
                                                                                           0 -> takeChase icons 2 Blue
                                                                                           1 -> takeChase icons 1 White
                                                                                           2 -> takeChase icons 1 Blue
                                                                                           _ -> takeChase icons 2 White

                                                                           |otherwise= takesolid icons

ghostchanger (Ghost gx gy _ gis _ _ _ _ _ _  _ Frighten _ _ _ ft) status _ | (ft `div` 3) `mod'` 2 == 0 && status == Playing = takeChase gis 1 Blue
                                                                          | otherwise = takeChase gis 2 Blue
ghostchanger (Ghost gx gy _ gis _ _ _ _ d _  _ _ _ _ _ _) status gt       | (gt `div` 3) `mod'` 2 == 0 && status == Playing = takepic d 1 gis
                                                                     | otherwise = takepic d 2 gis


takeghost :: GhostType -> [Ghost] -> Ghost
takeghost ghost (g@(Ghost _ _ _ _ _ name _ _ _ _ _ _ _ _ _ _):gs) | name == ghost = g
                                                                | otherwise      =  takeghost ghost gs

takesolid :: [Animation] -> Picture
takesolid ((Solid k):xs)= k
takesolid (x:xs) = takesolid xs

takeChase :: [Animation]->Int -> GhostColor -> Picture
takeChase ((Scattermode c i pic ):xs) num col | c == col && i == num = pic
                                                | otherwise = takeChase xs num col
takeChase (_:xs) num col = takeChase xs num col

takepic :: Direction -> Int -> [Animation] -> Picture
takepic d i ((Animation di ti p ):xs) | di == d && ti == i = p
                                      | otherwise = takepic d i xs