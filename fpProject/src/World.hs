{-# XTypeSynonymInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
module World where

import Helper


import Objects.Objects
import Objects.Enemy
import Objects.Ships
import Objects.Projectiles
import Graphics.Gloss.Data.Picture
import Graphics.Gloss

data WorldState = MkWorldState { state :: State,
                                 elapsedTime :: Float,
                                 player :: PlayerShip,
                                 enemies :: [Enemy],
                                 projectiles :: [Projectile],
                                 grid            :: [[Block]],
                                 completed_x     :: Float}
data State = Unloaded | Playing | Paused | GameOver | Completed


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
                        (xp, yp) = getPosition (player wstate)
                        (x, y) = (fromInteger $ floor xp,fromInteger $ floor yp)
                        bss = grid wstate
                    in (xbound, ybound)

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


class Positioned a => RenderableObject a where
    getPicture   :: a -> Picture
    getColor     :: a -> Color 
    renderObject :: a -> WorldState -> Picture
    renderObject ob wstate  | inBound p b = Translate (x-xmin) (y-ymin) (color c pic)
                            | otherwise   = Blank
        where   b@(MkBound xmin xmax, MkBound ymin ymax) = mapBounds wstate
                p@(x, y) = getPosition ob
                c        = World.getColor ob
                pic      = World.getPicture ob

instance RenderableObject Enemy where
    getPicture (MkSuicideEnemy suicideShip) = World.getPicture suicideShip
    getPicture (MkGunEnemy gunShip) = World.getPicture gunShip
    getPicture (MkRocketEnemy rocketShip) = World.getPicture rocketShip

    getColor (MkSuicideEnemy suicideShip) = World.getColor suicideShip
    getColor (MkGunEnemy gunShip) = World.getColor gunShip
    getColor (MkRocketEnemy rocketShip) = World.getColor rocketShip

instance RenderableObject Ship where
    getColor ship = Objects.Ships.getColor ship
    getPicture ship = Objects.Ships.getPicture ship

instance RenderableObject PlayerShip where
    getPicture (MkPlayerShip ship) = World.getPicture ship
    getColor (MkPlayerShip ship) = World.getColor ship

instance RenderableObject GunShip where
    getPicture (MkGunShip ship) = World.getPicture ship
    getColor (MkGunShip ship) = World.getColor ship

instance RenderableObject SuicideShip where
    getPicture (MkSuicideShip ship _) = World.getPicture ship
    getColor (MkSuicideShip ship _) = World.getColor ship

instance RenderableObject RocketShip where
    getPicture (MkRocketShip ship) = World.getPicture ship
    getColor (MkRocketShip ship) = World.getColor ship