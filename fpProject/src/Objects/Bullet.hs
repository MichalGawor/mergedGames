{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
module Objects.Bullet where 

import Graphics.Gloss

import Kinematics
import Plane
import Objects.Objects



-- ## Bullet 
data Bullet = MkBullet { position::Point,
                         velocity::Velocity, 
                         damage::Damage }

instance Moveable Bullet where
    move :: Bullet -> Target Point -> Maybe Bullet
    -- behaves independent on the targets due to innertia of the bullet 
    move bullet@(MkBullet { position, velocity }) _ = case uniformLinearMotion position velocity of
                                                          newPosition | isInScreen newPosition -> Just bullet{ position=newPosition, velocity=velocity}
                                                          otherwise -> Nothing

instance Killable Bullet where 
    -- just so Projectile can derive Killable
    takeDamage :: Bullet -> Damage -> Maybe Bullet
    takeDamage bullet _ = Just bullet

instance RenderableM Bullet where
    renderM bullet = uncurry Graphics.Gloss.translate (position bullet) (color red (Circle 0.1))

instance Positioned Bullet where
    getPosition bullet = position bullet
    setPosition bullet@( MkBullet {position}) newPosition = bullet{ position=newPosition }
