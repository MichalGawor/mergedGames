{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
module Objects.Objects where

import Graphics.Gloss

import Kinematics
import Plane

-- # Shared types
type Damage = Float
type Hp = Float

-- # Every object is an object
class Object a where
    isKillable :: a -> Bool

-- # Object classes
-- # every object can be moved
class Moveable a where
    move :: a -> Target Point -> Maybe a

-- # every object can be rendered
class RenderableM a where
    renderM :: a -> Picture


-- # some objects can be despawned
class Killable a where 
    takeDamage :: a -> Damage -> Maybe a

class Positioned a where
    getPosition :: a -> Point
    setPosition :: a -> Point -> a

class Positioned a => HitBox a where
    getWidth, getHeight :: a -> Float
    getBorders :: a -> (Point, Point, Point, Point)
    contains :: a -> Point -> Bool

    getBorders i = ((x, y), (x + w, y), (x, y + h), (x + w, y + h))
        where
            (x, y) = getPosition i
            w      = getWidth i
            h      = getHeight i

    contains i (px, py) = px >= x1 && px <= x2 && py >= y1 && py <= y2
        where ((x1, y1), _, _, (x2, y2)) = getBorders i

intersects :: (HitBox a, HitBox b) => a -> b -> Bool
intersects i1 i2 = let (p1, p2, p3, p4) = getBorders i1 
                        in contains i2 p1 
                        || contains i2 p2 
                        || contains i2 p3 
                        || contains i2 p4