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
    renderM (MkBulletProjectile bullet) = renderM bullet
        -- uncurry Graphics.Gloss.translate position (color getColor getPicture)

instance Moveable Projectile where
    move (MkBulletProjectile bullet) target = case move bullet target of 
                                                   Just bullet' -> Just (MkBulletProjectile (bullet'))
                                                   Nothing -> Nothing
    move (MkRocketProjectile rocket) target = case move rocket target of
                                                   Just rocket' -> Just (MkRocketProjectile (rocket'))
                                                   Nothing -> Nothing

instance HitBox Projectile where
    getWidth  _ = 1
    getHeight _ = 1

instance Positioned Projectile where
    getPosition (MkBulletProjectile bullet) = getPosition bullet
    getPosition (MkRocketProjectile rocket) = getPosition rocket
    setPosition (MkBulletProjectile bullet) position = MkBulletProjectile (setPosition bullet position)
    setPosition (MkRocketProjectile rocket) position = MkRocketProjectile (setPosition rocket position)


getProjectileDamage :: Projectile -> Damage
getProjectileDamage (MkBulletProjectile bullet) = Objects.Bullet.damage bullet
getProjectileDamage (MkRocketProjectile rocket) = Objects.Rocket.damage rocket
