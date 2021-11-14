{-# XTypeSynonymInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

module Objects.Weapon where

import Plane
import Objects.Projectiles


-- # Basic types
type Counter = Int -- tracks 
type StepsPerShoot = Int

-- # Weapons
data Weapon = MkWeapon Projectile StepsPerShoot Counter | NoWeapon 

class Shootable a where
    shoot :: a -> (a, Maybe Projectile)

instance Shootable Weapon where
    shoot :: Weapon -> (Weapon, Maybe Projectile)
    shoot NoWeapon = (NoWeapon, Nothing)
    shoot (MkWeapon projectile stepsPerShoot counter) | counter == stepsPerShoot = (MkWeapon projectile stepsPerShoot 0, Just projectile)
                                                      | otherwise = (MkWeapon projectile stepsPerShoot (counter+1), Nothing)
