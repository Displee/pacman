module Controller where

import Model

import System.IO.Unsafe
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe (fromJust, isJust)
import GameStateUtils
import GridUtils
import System.Random (randomRIO)

-- | Handle the game loop
loop :: Float -> GameState -> IO GameState
loop seconds gs@(GameState m@(Maze _ _ level ts) s p@(Player pi pa pst px py t d nd v sc li dt) g gt jt wt) = case s of
                                             Starting  -> do
                                                             let status | s == Starting && gt >= startTimeGameTicks = Playing
                                                                        | otherwise = s
                                                             let test = (\(Maze _ _ _ tiles) -> tiles)
                                                             return $ GameState m status (Player pi pa pst px py t d nd v sc li dt) g (gt + 1) jt wt
                                             Playing   -> do
                                                             let winGame = checkWinGame ts
                                                             let targetedGhosts = targetLocation' d t g jt
                                                             let player = handlePlayerMovement m p
                                                             let updatedMazePlayer = handleScorePlayer m player
                                                             let updatedMaze = fst updatedMazePlayer
                                                             let updatedPlayer = snd updatedMazePlayer
                                                             let frightenghosts = frigthenmodecheck m updatedPlayer targetedGhosts
                                                             let ghostsmode = modechanger' frightenghosts gt
                                                             let dirghosts = directionGhosts ghostsmode updatedMaze sc jt
                                                             let ghosts = map  (\x -> (handleGhostMovement (mazeChecker x m jt sc) x)) dirghosts
                                                             let checkDeadPlayerGhosts = checkPlayerDead updatedPlayer ghosts gt
                                                             let checkedPlayer = fst checkDeadPlayerGhosts
                                                             let checkedGhosts = snd checkDeadPlayerGhosts
                                                             let status | winGame = Paused
                                                                        | getPlayerLifes checkedPlayer < 0 = GameOver
                                                                        | getPlayerDeadTicks checkedPlayer == gt = Paused
                                                                        | otherwise = s
                                                             let winGameTick | winGame = gt
                                                                             | otherwise = wt
                                                             let update = frightenmodeexc (GameState updatedMaze status checkedPlayer checkedGhosts gt jt winGameTick)
                                                             return $ update
                                             Paused    -> do
                                                             let winGame = wt /= 0 && gt - wt >= nextLevelTick
                                                             let respawn = dt /= 0 && gt - dt >= respawnTick
                                                             let playerRespawn | respawn = respawnPlayer p
                                                                               | otherwise = p
                                                             let ghostsRespawn | respawn = map respawnGhost g
                                                                               | otherwise = g
                                                             let finalGs | respawn = GameState m Starting playerRespawn ghostsRespawn 0 jt wt
                                                                         | otherwise = GameState m s playerRespawn ghostsRespawn (gt + 1) jt wt
                                                             if winGame then createGameState (level + 1) else return finalGs
                                             GameOver  -> do
                                                             let restartLevel = gt - dt >= restartLevelTicks
                                                             if restartLevel then
                                                                                  createGameState level
                                                                            else
                                                                                 return $ GameState m s p g (gt + 1) jt wt

allPosDirec :: [Direction]
allPosDirec= [North,South,West,East]

directionGhosts :: [Ghost] -> Maze ->Int-> (Tile,Tile)-> [Ghost]
directionGhosts [] _ _ _ = []
directionGhosts (g:gs) m score jt =  (move g  (validmoves (mazeChecker g m jt score) g allPosDirec)): directionGhosts gs m score jt

mazeChecker :: Ghost ->Maze-> (Tile,Tile) ->Int -> Maze
mazeChecker g@(Ghost _ _ _ _ _ Pinky _ _ _ _ _ _ _ _ _ _)  maze jt _    | isInJail jt  g               = removeJailDoorsFromMaze maze
                                                                        | otherwise                    = maze
mazeChecker g@(Ghost _ _ _ _ _ Inky _ _ _ _ _ _ _ _ _ _)   maze jt score| isInJail jt g && score > 300 =  removeJailDoorsFromMaze maze
                                                                        | otherwise                    = maze
mazeChecker g@(Ghost _ _ _ _ _ Clyde _ _ _ _ _ _ _ _ _ _)  maze jt score| isInJail jt g && score > 700 =  removeJailDoorsFromMaze maze
                                                                        | otherwise                    = maze
mazeChecker g@(Ghost _ _ _ _ _ Blinky _ _ _ _ _ _ _ _ _ _)  maze _ _   = maze

move :: Ghost -> [Direction] -> Ghost
move g [] = g
move g@(Ghost gx gy gi gis gst gt posg@(Tile gtx gty tt) pl dg ngd vg m tlx tly ct ft) [x]  =  (Ghost gx gy gi gis gst gt posg  dg dg (Just x) vg m tlx tly ct ft)
move g@(Ghost gx gy gi gis gst gt posg@(Tile gtx gty tt) pl dg ngd vg Frighten tlx tly ct ft) xs= (Ghost gx gy gi gis gst gt posg  dg dg (Just (xs !!(unsafePerformIO(randomnumber ((length xs)-1))))) vg Frighten tlx tly ct ft)
move g@(Ghost gx gy gi gis gst gt posg@(Tile gtx gty tt) pl dg ngd vg m tlx tly ct ft) (d:ds) | d == North  && gty <= tly  =    (Ghost gx gy gi gis gst gt posg  dg dg (Just North) vg m tlx tly ct ft)
                                                                                              | d == West && gtx >= tlx  =      (Ghost gx gy gi gis gst gt  posg  dg dg (Just West) vg m tlx tly ct ft)
                                                                                              | d == East  && gtx <= tlx =    (Ghost gx gy gi gis gst gt  posg  dg dg (Just East) vg m tlx tly ct ft)
                                                                                              | d == South && gty >= tly =    (Ghost gx gy gi gis gst gt  posg  dg dg (Just South) vg m tlx tly ct ft)
                                                                                              | otherwise = move g ds

randomnumber :: Int -> IO Int
randomnumber k = do randomRIO (0, k)

validmoves :: Maze -> Ghost-> [Direction] -> [Direction]
validmoves _  _ [] = []
validmoves maze  g@(Ghost gx gy gi gis gst gt posg@(Tile gtx gty tt) pl dg ngd vg m tlx tly ct ft) (d:ds) | d==North && (not(pl == South)) && (not(dg == South))  &&  isOnTile gx gy gtx gty && not  (tileInFrontIsBlocked maze gtx gty North)    = North: validmoves maze g ds
                                                                                                          | d==South&& (not(pl == North)) && (not(dg == North)) &&  isOnTile gx gy gtx gty && not  (tileInFrontIsBlocked maze gtx gty South)     = South: validmoves maze g ds
                                                                                                          | d==West && (not(pl == East) ) && (not(dg == East))  &&   isOnTile gx gy gtx gty && not  (tileInFrontIsBlocked maze gtx gty West)     = West : validmoves maze g ds
                                                                                                          | d==East && (not(pl == West))  && (not(dg == West))   &&   isOnTile gx gy gtx gty && not  (tileInFrontIsBlocked maze gtx gty East)    = East : validmoves maze g ds
                                                                                                          |otherwise =  validmoves maze g ds

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate@(GameState m s (Player _ _ _ px py pt d nd v sc li dt) g gt jt wt) = return (inputKey e gstate)

handlePlayerMovement :: Maze -> Player -> Player
handlePlayerMovement m (Player pic pi pst px py (Tile _ _ tt) d nd v sc li dt) = Player pic pi pst newPx newPy (Tile tileX tileY tt) dir nextDir v sc li dt
                                                                       where
                                                                             tileX = screenXToTile px
                                                                             tileY = screenYToTile py
                                                                             onTile = isOnTile px py tileX tileY
                                                                             tileInFrontIsB = tileInFrontIsBlocked m tileX tileY d
                                                                             dir | onTile && isJust nd && not (tileInFrontIsBlocked m tileX tileY (fromJust nd)) = fromJust nd
                                                                                 | otherwise = d
                                                                             sdv = screenDirectionVector dir
                                                                             newPx | tileInFrontIsB && onTile = px
                                                                                   | otherwise = px + fromIntegral(fst sdv)
                                                                             newPy | tileInFrontIsB && onTile = py
                                                                                   | otherwise = py + fromIntegral(snd sdv)
                                                                             nextDir | onTile && isJust nd && not (tileInFrontIsBlocked m tileX tileY (fromJust nd)) = Nothing
                                                                                     | otherwise = nd

handleGhostMovement :: Maze -> Ghost -> Ghost
handleGhostMovement m (Ghost gx gy gi gis gst gt (Tile _ _ tt) pl d nd v mo tlx tly gct ft) = Ghost newGx newWGy gi gis gst gt (Tile tileX tileY tt) pl dir nextDir v mo tlx tly gct ft
                                                        where
                                                             tileX = screenXToTile gx
                                                             tileY = screenYToTile gy
                                                             onTile = isOnTile gx gy tileX tileY
                                                             tileInFrontIsB= tileInFrontIsBlocked m tileX tileY d
                                                             dir | onTile && isJust nd && not (tileInFrontIsBlocked m tileX tileY (fromJust nd)) = fromJust nd
                                                                 | otherwise = d
                                                             sdv = screenDirectionVector dir
                                                             newGx | tileInFrontIsB && onTile = gx
                                                                   | otherwise = gx + fromIntegral(fst sdv)
                                                             newWGy | tileInFrontIsB && onTile = gy
                                                                    | otherwise = gy + fromIntegral(snd sdv)
                                                             nextDir | onTile && isJust nd && not (tileInFrontIsBlocked m tileX tileY (fromJust nd)) = Nothing
                                                                     | otherwise = nd

handleScorePlayer :: Maze -> Player -> (Maze, Player)
handleScorePlayer m@(Maze w h level tiles) p@(Player i pi pst px py (Tile tx ty tt) d nd v s l dt) = (Maze w h level updatedTiles, Player i pi pst px py (Tile tx ty tt) d nd v playerScore l dt)
                                                                  where
                                                                        tif = tileInFront m d tx ty
                                                                        tift = getTileType tif
                                                                        nearObjective = isNearTile px py tif && (tift == Dot || tift == FlashingDot)
                                                                        updatedTiles | nearObjective = modifyTileType tif NormalTile : oldTiles
                                                                                     | otherwise = tiles
                                                                                     where
                                                                                           oldTiles = filter (/= tif) tiles
                                                                        playerScore | nearObjective = s + tileScore tif
                                                                                    | otherwise = s

targetLocation' :: Direction-> Tile -> [Ghost]->(Tile,Tile) -> [Ghost]
targetLocation' _ _ [] _= []
targetLocation' d t@(Tile x y _) (g@(Ghost _ _ _ _ _ Inky _ _ _ _ _ _ _ _ _ _):gs)   jt  = targetInky d x y g  (takeghostloc Blinky (g:gs)) jt: targetLocation' d t gs jt
targetLocation' d t@(Tile x y _) (g@(Ghost _ _ _ _ _ Blinky _ _ _ _ _ _ _ _ _ _):gs) jt  = targetBlinky d x y g : targetLocation' d t gs jt
targetLocation' d t@(Tile x y _) (g@(Ghost _ _ _ _ _ Clyde _ _ _ _ _ _ _ _ _ _):gs)  jt  = targetClyde d x y g jt: targetLocation' d t gs jt
targetLocation' d t@(Tile x y _) (g@(Ghost _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _):gs)      jt  = targetPinky d x y g jt: targetLocation' d t gs jt

--The target location of Blinky is the location of Pac-man.
targetBlinky :: Direction->Int -> Int -> Ghost -> Ghost
targetBlinky d x y g@(Ghost gx gy gi gis gst gt pg pl dg nd vg m ttx tty ct ft) | m == Chase = Ghost gx gy gi gis gst gt pg pl dg nd vg m x y ct ft
                                                                                | m == Scatter = Ghost gx gy gi gis gst gt pg pl dg nd vg m 28 1 ct ft
                                                                                | otherwise = g
--The target location of Pinky is 4 tiles in front of pac-man in the direction it is going
targetPinky :: Direction -> Int -> Int -> Ghost ->(Tile,Tile)-> Ghost
targetPinky d x y g@(Ghost gx gy gi gis gst@(Tile tx ty tt) gt pg pl dg nd vg m ttx tty ct ft)jt | isInJail jt g = (Ghost gx gy gi gis gst gt pg pl dg nd vg m tx (ty+4) ct ft)
                                                                                                 | m == Chase = case d of
                                                                                                            North -> Ghost gx gy gi gis gst gt pg pl dg nd vg m x (y+4) ct ft
                                                                                                            South -> Ghost gx gy gi gis gst gt pg pl dg nd vg m x (y-4) ct ft
                                                                                                            West  -> Ghost gx gy gi gis gst gt pg pl dg nd vg m (x-4) y ct ft
                                                                                                            _     -> Ghost gx gy gi gis gst gt pg pl dg nd vg m (x+4) y ct ft
                                                                                                 | m == Scatter = (Ghost gx gy gi gis gst gt pg pl dg nd vg m 4 1 ct ft)
                                                                                                 | otherwise = g
--When clyde is in a proximity of 8 tiles of Pac-man the target location is pac-man itself. Otherwise,the target location is the scatter location.
targetClyde :: Direction -> Int -> Int -> Ghost->(Tile,Tile) -> Ghost
targetClyde _ x y g@(Ghost gx gy gi gis gst gt t@(Tile tx ty tt) pl dg nd vg m ttx tty ct ft) jt  | isInJail jt g = Ghost gx gy gi gis gst gt t pl dg nd vg m tx (ty+4) ct ft
                                                                                                  | m == Chase  = proxof8tiles
                                                                                                  | m == Scatter = (Ghost gx gy gi gis gst gt (Tile tx ty tt) pl dg nd vg m 1 36 ct ft)
                                                                                                  | otherwise    = (Ghost gx gy gi gis gst gt (Tile tx ty tt) pl dg nd vg m ttx tty ct ft) where
                    proxof8tiles   | ((((x+y) >= (tx+ty)) && (((x+y) - (tx+ty)) <= 8)) ||  (((tx+ty) >= (x+y)) && (((tx+ty) - (x+y)) <= 8))) = (Ghost gx gy gi gis gst gt (Tile tx ty tt) pl dg nd vg m x y ct ft)
                                   | otherwise  =(Ghost gx gy gi gis gst gt (Tile tx ty tt)  pl dg nd vg m 1 36 ct ft)

--The target location of Inky is 2 tiles in the direction pac-man is going.
targetInky :: Direction -> Int -> Int -> Ghost -> Tile ->(Tile,Tile)-> Ghost
targetInky d x y g@(Ghost gx gy gi gis gst@(Tile inkytx inkyty inkytt) gt pg pl dg nd vg m ttx tty ct ft) tile@(Tile tx ty tt)  jt| isInJail jt g = Ghost gx gy gi gis gst gt pg pl dg nd vg m inkytx (inkyty+4) ct ft
                                                                                                                                  | m == Chase = case d of
                                                                                                                                    North -> Ghost gx gy gi gis gst gt pg pl dg nd vg m (targetInky' tx x)     (targetInky' ty (y+2)) ct ft
                                                                                                                                    South -> Ghost gx gy gi gis gst gt pg pl dg nd vg m (targetInky' tx x)     (targetInky' ty (y+2)) ct ft
                                                                                                                                    West  -> Ghost gx gy gi gis gst gt pg pl dg nd vg m (targetInky' tx (x-2)) (targetInky' ty y) ct ft
                                                                                                                                    _     -> Ghost gx gy gi gis gst gt pg pl  dg nd vg m (targetInky' tx (x+2))(targetInky' ty y) ct ft
                                                                                                                                  | m == Scatter = (Ghost gx gy gi gis gst gt pg pl dg nd vg m 28 36 ct ft)
                                                                                                                                  | otherwise = g

targetInky' :: Int ->Int->Int
targetInky'  tx x= tx+(tx-x)*2

tileInFront :: Maze -> Direction -> Int -> Int -> Tile
tileInFront (Maze _ _ _ xs) d cx cy = head $ filter (\(Tile tx ty _) -> tx == (cx + fst v) && ty == (cy + snd v)) xs
                                      where
                                            v = tileDirectionVector d

getTileType :: Tile -> TileType
getTileType (Tile _ _ tt) = tt

tileInFrontIsBlocked :: Maze -> Int -> Int -> Direction -> Bool
tileInFrontIsBlocked m tx ty d = tift == Wall || tift == JailDoor where tift = getTileType (tileInFront m d tx ty)

isOnTile :: Float -> Float -> Int -> Int -> Bool
isOnTile sx sy tx ty = sx == tileToScreenX tx && sy == tileToScreenY ty

isNearTile :: Float -> Float -> Tile -> Bool
isNearTile sx sy (Tile x y _) = x == screenXToTile sx && y == screenYToTile sy

getNearGhosts :: Float -> Float -> [Ghost] -> [Ghost]
getNearGhosts _ _ [] = []
getNearGhosts sx sy (x:xs) | sxDiff < nearDistance && sxDiff > (-nearDistance) &&
                             syDiff < nearDistance && syDiff > (-nearDistance) = x : getNearGhosts sx sy xs
                           | otherwise = getNearGhosts sx sy xs
                           where
                                 pos = (\(Ghost gx gy _ _ _ _ _ _ _ _ _ _ _ _ _ _) -> (gx, gy)) x
                                 sxDiff = sx - fst pos
                                 syDiff = sy - snd pos
                                 nearDistance = 5

isNearGhost :: Float -> Float -> [Ghost] -> Bool
isNearGhost _ _ [] = False
isNearGhost sx sy (x:xs) = not vulnerable && (sxDiff < nearDistance && sxDiff > (-nearDistance) &&
                            syDiff < nearDistance && syDiff > (-nearDistance)) || isNearGhost sx sy xs
                           where
                                 pos = (\(Ghost gx gy _ _ _ _ _ _ _ _ _ _ _ _ _ _) -> (gx, gy)) x
                                 mode = (\(Ghost _ _ _ _ _ _ _ _ _ _ _ m _ _ _ _) -> m) x
                                 vulnerable = mode == Frighten || mode == Frightenwhite
                                 sxDiff = sx - fst pos
                                 syDiff = sy - snd pos
                                 nearDistance = 5

isInJail :: (Tile, Tile) -> Ghost -> Bool
isInJail (Tile topLeftX topLeftY _, Tile bottomRightX bottomRightY _) (Ghost _ _ _ _ _ _ (Tile tx ty _) _ _ _ _ _ _ _ _ _) = tx >= topLeftX && tx <= bottomRightX && ty >= topLeftY && ty <= bottomRightY

removeJailDoorsFromMaze (Maze w h l xs) = Maze w h l $ convertedJailDoorTiles ++ filter (\(Tile _ _ tt) -> tt /= JailDoor) xs
                                          where
                                                jailDoorTiles = filter (\(Tile _ _ tt) -> tt == JailDoor) xs
                                                convertedJailDoorTiles = map (\(Tile tx ty _) -> Tile tx ty NormalTile) jailDoorTiles

tileScore :: Tile -> Int
tileScore (Tile _ _ NormalTile) = 0
tileScore (Tile _ _ Wall) = 0
tileScore (Tile _ _ JailDoor) = 0
tileScore (Tile _ _ Dot) = 10
tileScore (Tile _ _ FlashingDot) = 50

modifyTileType :: Tile -> TileType -> Tile
modifyTileType (Tile tx ty _) = Tile tx ty

checkPlayerDead :: Player -> [Ghost] -> Int -> (Player, [Ghost])
checkPlayerDead p@(Player pi pis pst px py pp d nd v sc li dt) ghosts gt | null nearGhosts = (p, ghosts)
                                                                         | length nearGhosts /= length vulnerableGhosts = (Player pi pis pst px py pp d nd v sc (li - 1) gt, ghosts)
                                                                         | otherwise = (Player pi pis pst px py pp d nd v (sc + 200) li dt, newGhosts)
                                                                         where
                                                                               nearGhosts = getNearGhosts px py ghosts
                                                                               vulnerableGhosts = filter (\(Ghost _ _ _ _ _ _ _ _ _ _ _ m _ _ _ _) -> m == Frighten || m == Frightenwhite) nearGhosts
                                                                               newGhosts = filter (\(Ghost _ _ _ _ _ gt _ _ _ _ _ _ _ _ _ _) -> gt `notElem` map ghosttype vulnerableGhosts) ghosts ++ map respawnGhost vulnerableGhosts

getPlayerDeadTicks :: Player -> Int
getPlayerDeadTicks (Player pi pis pst px py pp d nd v sc li dt) = dt

getPlayerLifes :: Player -> Int
getPlayerLifes (Player pi pis pst px py pp d nd v sc li dt) = li

respawnPlayer :: Player -> Player
respawnPlayer (Player pi pis pst@(Tile psx psy _) _ _ pt d _ v sc li _) = Player pi pis pst newPx newPy pt d Nothing v sc li 0
                                                             where
                                                                   newPx = tileToScreenX psx
                                                                   newPy = tileToScreenY psy

respawnGhost :: Ghost -> Ghost
respawnGhost (Ghost gx gy gi gis gst@(Tile gsx gsy _) gt _ pl dg _ vg m ttx tty ct ft) = Ghost newGx newGy gi gis gst gt gst pl dg Nothing vg m ttx tty 0 0
                                                             where
                                                                   newGx = tileToScreenX gsx
                                                                   newGy = tileToScreenY gsy

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyUp) _ _ _) (GameState m s (Player pi pis pst px py pp d nd v sc li dt) g gt jt wt) = GameState m s (Player pi pis pst px py pp d (Just North) v sc li dt) g gt jt wt
inputKey (EventKey (SpecialKey KeyDown) _ _ _) (GameState m s (Player pi pis pst px py pp d nd v sc li dt) g gt jt wt) = GameState m s (Player pi pis pst px py pp d (Just South) v sc li dt) g gt jt wt
inputKey (EventKey (SpecialKey KeyRight) _ _ _) (GameState m s (Player pi pis pst px py pp d nd v sc li dt) g gt jt wt) = GameState m s (Player pi pis pst px py pp d (Just East) v sc li dt) g gt jt wt
inputKey (EventKey (SpecialKey KeyLeft) _ _ _) (GameState m s (Player pi pis pst px py pp d nd v sc li dt) g gt jt wt) = GameState m s (Player pi pis pst px py pp d (Just West) v sc li dt) g gt jt wt
inputKey _ gstate = gstate

tileDirectionVector :: Direction -> (Int, Int)
tileDirectionVector North = (0, -1)
tileDirectionVector South = (0, 1)
tileDirectionVector East = (1, 0)
tileDirectionVector West = (-1, 0)

screenDirectionVector :: Direction -> (Int, Int)
screenDirectionVector North = (0, 1)
screenDirectionVector South = (0, -1)
screenDirectionVector East = (1, 0)
screenDirectionVector West = (-1, 0)

modechanger' :: [Ghost] -> Int -> [Ghost]
modechanger' [] _ = []
modechanger' (g:gs) tick = modechanger g tick : modechanger'  gs tick

modechanger:: Ghost -> Int -> Ghost
modechanger g@(Ghost _ _ _ _ _ _ _ _ _ _ _ Frighten _ _ _ _) _= g
modechanger g@(Ghost _ _ _ _ _ _ _ _ _ _ _ Frightenwhite _ _ _ _) _= g
modechanger  g tc    | tc < 420                 = changemode g Scatter
                     |420 > tc  && tc < 1620    = changemode g Chase
                     |1620 > tc && tc <2040     = changemode g Scatter
                     |2040 > tc && tc <3240     = changemode g Chase
                     |3240 > tc && tc <3540     = changemode g Scatter
                     |3540> tc  && tc <4740     = changemode g Chase
                     |4740> tc  && tc <5040     = changemode g Scatter
                     |otherwise                 = changemode g Chase

changemodes :: [Ghost] -> Mode -> [Ghost]
changemodes gs  mode  = map (\x -> changemode x mode ) gs

changemode :: Ghost -> Mode -> Ghost
changemode (Ghost gx gy gi gis gst gt posg pl dg ngd vg Frightenwhite tlx tly ct ft) mode = (Ghost gx gy gi gis gst gt posg pl dg ngd vg mode tlx tly ct 0)
changemode (Ghost gx gy gi gis gst gt posg pl dg ngd vg _ tlx tly ct ft) mode = (Ghost gx gy gi gis gst gt posg pl dg ngd vg mode tlx tly ct ft)

incrementghosttick :: [Ghost] -> [Ghost]
incrementghosttick [] = []
incrementghosttick  ((Ghost gx gy gi gis gst gt posg pl dg ngd vg mode tlx tly ct ft): gs) =  Ghost gx gy gi gis gst gt posg pl dg ngd vg mode tlx tly ct (ft+1) : incrementghosttick gs

frightenmodeexc :: GameState -> GameState
frightenmodeexc (GameState m s p (g@(Ghost gx gy gi gis gst typ posg pl dg ngd vg Frighten tlx tly ct ft):gs) gt jt wt)      | 300< ft   = (GameState m s p (incrementghosttick (changemodes (g:gs) Frightenwhite) ) gt jt wt)
                                                                                                                   | otherwise = (GameState m s p (incrementghosttick (g:gs)) gt jt wt)
frightenmodeexc (GameState m s p (g@(Ghost gx gy gi gis gst typ posg pl dg ngd vg Frightenwhite tlx tly ct ft):gs) gt jt wt) | ft> 600 = (GameState m s p (changemodes (g:gs) Chase) (gt+1) jt wt)
                                                                                                                   | otherwise = (GameState m s p (incrementghosttick (g:gs)) gt jt wt)
frightenmodeexc  (GameState m s p g gt jt wt) = (GameState m s p g (gt+1) jt wt)


frigthenmodecheck :: Maze -> Player -> [Ghost] -> [Ghost]
frigthenmodecheck m p g | onflashingdot m p =  changemodes g Frighten 
                        | otherwise          = g


onflashingdot :: Maze -> Player -> Bool
onflashingdot m p@(Player i pi pst px py (Tile tx ty tt) d nd v s l dt) =  isNearTile px py tif &&  tift == FlashingDot  where
                                                                        tif = tileInFront m d tx ty
                                                                        tift = getTileType tif

takeghostloc :: GhostType -> [Ghost] -> Tile
takeghostloc ghost (g@(Ghost _ _ _ _ _ name t _ _ _ _ _ _ _ _ _):gs) | name == ghost = t
                                                                   | otherwise      =  takeghostloc ghost gs

createGameState :: Int -> IO GameState
createGameState level = do
                          levelContent <- readFile ("./data/level_" ++ show level ++ ".txt")
                          let maze = Maze (fst mt + 1) (snd mt + 1) level tiles
                                     where
                                           tiles = gridMaker 1 1 levelContent
                                           maxTile acc [] = acc
                                           maxTile acc@(accx, accy) ((Tile xx xy __):xs) | xx >= accx && xy >= accy = maxTile (xx, xy) xs
                                                                                         | otherwise = maxTile acc xs
                                           mt = maxTile (0, 0) tiles
                          let playerPosition = (\(Tile tx ty _) -> (tx, ty)) $ readPlayer 1 1 levelContent
                          player <- uncurry (createPlayer maze) playerPosition
                          pinky <- readGhost 1 1 'P' levelContent
                          inky <- readGhost 1 1 'I' levelContent
                          blinky <- readGhost 1 1 'B' levelContent
                          clyde <- readGhost 1 1 'C' levelContent
                          let ghosts = map fromJust [pinky, inky, blinky, clyde]
                          let jailTiles = findJailPositions 1 1 (Tile 0 0 NormalTile, Tile 0 0 NormalTile) levelContent
                          return (GameState maze Starting player ghosts 0 jailTiles 0)

checkWinGame :: [Tile] -> Bool
checkWinGame [] = True
checkWinGame ((Tile _ _ tt):xs) | tt /= Dot && tt /= FlashingDot = checkWinGame xs
                                | otherwise = False