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
class Renderable a where
    render :: a -> Picture


-- # some objects can be despawned
class Killable a where 
    takeDamage :: a -> Damage -> Maybe a

class Positioned a where
    getPosition :: a -> Point
    setPosition :: a -> Point -> a
