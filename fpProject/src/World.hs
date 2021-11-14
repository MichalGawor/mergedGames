module World where

import Helper
import Graphics.Gloss.Data.Picture
import Graphics.Gloss

data WorldState = MkWorldState {player          :: Player,
                                enemies         :: [Enemy],
                                state           :: State,
                                grid            :: [[Block]],
                                completed_x     :: Float}

data State = Unloaded | Playing | Paused | GameOver | Completed

data Enemy = MkEnemy {enemy_position :: Point}     

data Player = MkPlayer { player_position  :: Point,
                         player_lives     :: Int,
                         needs_respawn    :: Bool}

data Block = MkBlock {block_position :: Point,
                      block_type     :: BlockType,
                      block_picture  :: Picture }

data BlockType = Solid | Void

getBlock :: [[Block]] -> Point -> Block
getBlock = undefined

hitsBlock :: [[Block]] -> Point -> Bool
hitsBlock = undefined

data Bound = MkBound Float Float

inBound :: Point -> (Bound, Bound) -> Bool
inBound (x, y) (MkBound xmin xmax, MkBound ymin ymax) = x >= xmin && x <= xmax &&  y >= ymin && y <= ymax

mapBounds :: WorldState -> (Bound, Bound)
mapBounds wstate = let  xbound  | x - 20 <= 1    = MkBound 0 42
                                | x + 20 > mapw = MkBound (mapw - 42) mapw
                                | otherwise      = MkBound (x - 22) (x + 22)
                        ybound  | y - 10 <= 1    = MkBound 0 22
                                | y + 10 > maph = MkBound (maph - 22) maph
                                | otherwise      = MkBound (y - 12) (y + 12)
                        mapw = fromIntegral . length . head $ bss
                        maph = fromIntegral $ length bss
                        (xp, yp) = player_position (player wstate)   
                        (x, y) = (fromInteger $ floor xp,fromInteger $ floor yp)
                        bss = grid wstate
                    in (xbound, ybound)

class HitBox a => Moveable a where
    getVelocity    :: a -> Velocity
    setVelocity    :: a -> Velocity -> a
    getSpeed       :: a -> Float

    move           :: a -> [[Block]] -> a
    move i bss = undefined
                        
    newpos         :: a -> Point
    newpos i = undefined

    isMoveable     :: a -> Point -> [[Block]] -> Bool
    isMoveable i (x, y) bss = undefined

data Health = Dead | Alive Int

instance Eq Health where
    Dead == Dead = True
    Alive x == Alive y = x == y
    _ == _ = False

instance Show Health where
    show (Alive x) = show x
    show Dead = "Dead"

class Healthy a where
    initialHealth :: a -> Health
    getHealth :: a -> Health
    setHealth :: a -> Health -> a
    getLives :: a -> Int
    setLives :: a -> Int -> a
 
    changeHealth :: a -> Int -> a
    changeHealth ob y = undefined -- check with lives if it should be alive or dead
    isDead :: a -> Bool
    isDead ob = case getHealth ob of
                    Dead -> True
                    _    -> False

class (Renderable a, Positioned a) => RenderableObject a where
    renderObject :: a -> WorldState -> Picture
    renderObject ob wstate  | inBound p b = Translate (x-xmin) (y-ymin) pic
                            | otherwise   = Blank
        where   b@(MkBound xmin xmax, MkBound ymin ymax) = mapBounds wstate
                p@(x, y) = getPosition ob
                pic = render ob

class (Moveable a, Healthy a) => UpdatableObject a where
    updateObject :: a -> Float -> WorldState -> a
    updateObject ob secs wstate | isDead ob = ob
                                | otherwise = ob''
                        where ob'   = move ob (grid wstate)
                              ob'' = customUpdate ob' secs wstate
    customUpdate :: a -> Float -> WorldState -> a