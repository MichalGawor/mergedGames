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
    renderM projectile = undefined
        --uncurry Graphics.Gloss.translate position (color getColor getPicture)


-- ## Projectiles 


