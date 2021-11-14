{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

module Objects.Projectiles where

import Plane
import Objects.Bullet
import Objects.Rocket

-- # Basic types

data Projectile = MkBulletProjectile Bullet | MkRocketProjectile Rocket




-- ## Projectiles 


