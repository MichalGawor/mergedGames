module Level where

import World
import Grid
import Player
import Enemy
import Graphics.Gloss.Data.Picture

getLevelContent :: [String] -> (Player, [Enemy], Float, [[Block]])
getLevelContent fc = (initPlayer (getSpawn fc), getEnemies fc,
                                     getCompleted fc, getGrid fc)

-- still needs the first three paramerters of level which should be player position, enemy positions and final x-position to win the level

getSpawn :: [String] -> Point
getSpawn (x:_) = getSpawn' $ words x
    where getSpawn' [] = (0,0)
          getSpawn' (x:y:_) = (sToI x, sToI y)

getEnemies :: [String] -> [Enemy]
getEnemies (_:y:_) = map initEnemy (enemies $ words y)
    where enemies [] = []
          enemies (x:y:ys) = (sToI x, sToI y) : enemies ys

getCompleted :: [String] -> Float
getCompleted (_:_:z:_) = sToI z

getGrid :: [String] -> [[Block]]
getGrid (x:y:z:ls)= makeGrid ls

sToI :: String -> Float
sToI x = read x