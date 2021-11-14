{-# XTypeSynonymInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
module WorldScreen where

import World
import Helper
import Level
import Grid
import Objects.Player
import Objects.Objects
import Objects.Projectiles
import Objects.Enemy
import Objects.Ships
import Kinematics

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game

initWorldstate :: WorldState
initWorldstate = MkWorldState Unloaded 0.0 player [] [] [[]] 0.0  
    where
        player = initPlayerShip (0,0) 


instance Updatable WorldState where
      update secs wstate = wstate''
            where wstate' = case state wstate of
                              Playing -> movePhase wstate -- updating all props of worldstate player, enemies, state, spawns
                              _      -> wstate
                  wstate'' = wstate' -- identify collisions

identifyCollisions :: WorldState -> WorldState
identifyCollisions = undefined  
 
calculateDamageEnemies :: WorldState -> WorldState
calculateDamageEnemies = undefined

calculateDamagePlayer :: WorldState -> WorldState
calculateDamagePlayer = undefined

respawnPlayer :: WorldState -> WorldState
respawnPlayer = undefined -- if needed


instance Renderable WorldState where
      render wstate secs = case state wstate of
                  Playing -> Translate (-1) (-1) . Scale (1/20) (1/10) $ Pictures (
                                                concatMap (\blocks -> map (\block -> renderObject block wstate) blocks) (grid wstate) ++
                                                [renderObject (player wstate) wstate] ++
                                                map (\e -> renderObject e wstate) (enemies wstate))
                  _     -> Blank

instance HandleInput WorldState where
      handleInput wstate (EventKey (Char 'p') Up _ _) = wstate { state = Paused}
      handleInput wstate e = wstate { player = handleInput (player wstate) e}


loadLevel :: WorldState -> IO WorldState
loadLevel wstate = case state wstate of
                        Unloaded -> do
                              fileContent <- readFile "Level1.txt"
                              let (p, es, c, g) = getLevelContent (lines fileContent)
                              return $ wstate { player = p, enemies = es, completed_x = c, grid = g, state = Playing}
                        _ -> return wstate 

instance DoIO WorldState where
      doIO wstate = do
            wstate' <- loadLevel wstate
            let grid' = grid wstate'
            grid'' <- loadGrid grid' -- doIO for enemies, bullets, player, etc.
            return $ wstate' {grid = grid''}


    -- | Movement phase
movePhase :: WorldState -> WorldState
movePhase ws@(MkWorldState{ player, enemies, projectiles, elapsedTime}) = let homingTarget = getPosition player
                                                                              movedPlayer = huskPlayerFromMaybe (move player NoTarget)
                                                                              suicideEnemies = getSuicideEnemies enemies
                                                                              normalEnemies = getNormalEnemies enemies
                                                                              movedSuicideEnemies = mapMaybe (flip moveObject (MkTarget homingTarget)) suicideEnemies
                                                                              movedNormalEnemies = mapMaybe (flip moveObject NoTarget) normalEnemies
                                                                              movedEnemies = movedNormalEnemies ++ movedSuicideEnemies
                                                                              normalProjectiles = getNormalProjectiles projectiles
                                                                              homingProjectiles = getHomingProjectiles projectiles
                                                                              --movedNormalProjectiles = mapMaybe (flip moveObject NoTarget) normalProjectiles
                                                                              --movedHomingProjectiles = mapMaybe (flip moveObject homingTarget) homingProjectiles
                                                                              --movedProjectiles = movedNormalProjectiles ++ movedHomingProjectiles
                                                                              in ws{player=movedPlayer, enemies=movedEnemies, projectiles=projectiles, elapsedTime=elapsedTime}

                              

shootPhase :: WorldState -> WorldState
shootPhase gameModel = gameModel

collisionPhase :: WorldState -> WorldState
collisionPhase gameModel = gameModel

moveObject :: Moveable a => a -> Target Point -> Maybe a
moveObject object target = Objects.Objects.move object target 

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
