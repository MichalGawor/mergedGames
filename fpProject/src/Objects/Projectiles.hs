{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

module Objects.Projectiles where

import Objects.Objects
import Plane
import Objects.Bullet
import Objects.Rocket

-- # Basic types

data Projectile = MkBulletProjectile Bullet | MkRocketProjectile Rocket

instance RenderableM Projectile where
    renderM (MkBulletProjectile bullet) = render bullet
        -- uncurry Graphics.Gloss.translate position (color getColor getPicture)

instance Moveable Projectile where
    move (MkBulletProjectile bullet) target = case move bullet target of 
                                                    Just bullet' -> Just (MkBulletProjectile (bullet')
                                                    otherwise -> Nothing
    move (MkRocketProjectile rocket) target = case move rocket target of
                                                    Just rocket' -> Just (MkRocketProjectile (rocket'))
                                                    otherwise -> Nothing


