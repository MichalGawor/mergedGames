module WorldScreen where

import World
import Helper
import Level
import Grid
import Player

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game

initWorldstate :: WorldState
initWorldstate = MkWorldState player [] Unloaded [[]] 0 
    where
        player = initPlayer(0,0) 


instance Updatable WorldState where
      update secs wstate = wstate''
            where wstate' = case state wstate of
                              Playing -> wstate -- updating all props of worldstate player, enemies, state, spawns
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
