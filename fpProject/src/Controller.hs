-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Helper
import Model
import Plane
import Kinematics
import Objects.Enemy
import Objects.Objects
import Objects.Ships
import Objects.Weapon
import Objects.Projectiles

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.ViewPort

-- | Handle one iteration of the game
step :: Float -> GameModel -> IO GameModel
step secs gstate = do
      gstate' <- doIO gstate
      return $ update secs (movePhase gstate')


-- | Movement phase
movePhase :: WorldState -> WorldState
movePhase ws@(MkWorldState{ player, enemies, projectiles, timeElapsed}) = let homingTarget = getPosition player
                                                                              suicideEnemies = getSuicideEnemies enemies
                                                                              normalEnemies = getNormalEnemies enemies
                                                                              movedSuicideEnemies = mapMaybe (flip moveObject homingTarget) suicideEnemies
                                                                              movedNormalEnemies = mapMaybe (flip moveObject NoTarget) normalEnemies
                                                                              movedEnemies = movedNormalEnemies ++ movedSuicideEnemies
                                                                              normalProjectiles = getNormalProjectiles projectiles
                                                                              homingProjectiles = getHomingProjectiles projectiles
                                                                              --movedNormalProjectiles = mapMaybe (flip moveObject NoTarget) normalProjectiles
                                                                              --movedHomingProjectiles = mapMaybe (flip moveObject homingTarget) homingProjectiles
                                                                              --movedProjectiles = movedNormalProjectiles ++ movedHomingProjectiles
                                                                              movedPlayer = huskPlayerFromMaybe $ moveObject player NoTarget
                                                                              in ws{player=movedPlayer, enemies=movedEnemies, projectiles=projectiles, timeElapsed=timeElapsed}

                              

shootPhase :: WorldState -> WorldState
shootPhase gameModel = gameModel

collisionPhase :: WorldState -> WorldState
collisionPhase gameModel -> WorldState

moveObject :: Moveable a => a -> Target Point -> Maybe a
moveObject object target = move object target 

update :: ViewPort -> Float -> GameModel -> GameModel
update _ = gameStep

getPlayerPosition :: PlayerShip -> Point
getPlayerPosition (MkPlayerShip (Ship { Objects.Ships.position })) = position

huskPlayerFromMaybe :: Maybe PlayerShip -> PlayerShip
huskPlayerFromMaybe (Just playerShip) = playerShip

isSuicideEnemy :: Enemy -> Bool
isSuicideEnemy (MkSuicideEnemy _) = True
isSuicideEnemy _ = False

getSuicideEnemies :: [Enemy] -> [Enemy]
getSuicideEnemies enemies = filter isSuicideEnemy enemies

getNormalEnemies :: [Enemy] -> [Enemy]
getNormalEnemies enemies = filter (not . isSuicideEnemy) enemies

isHomingProjectile :: Projectile -> Bool
isHomingProjectile (MkRocketProjectile _) = True
isHomingProjectile _ = False

getHomingProjectiles :: [Projectile] -> [Projectile]
getHomingProjectiles projectiles = filter (isHomingProjectile) projectiles

getNormalProjectiles :: [Projectile] -> [Projectile]
getNormalProjectiles projectiles = filter (not . isHomingProjectile) projectiles


shootPhase = undefined
applyDamagePhase = undefined

-- | Handle user input
input :: Event -> GameModel -> IO GameModel
input event gstate = return $ handleInput gstate event 
