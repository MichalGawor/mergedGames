{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

module Weapon where

import Plane
import Projectiles


-- # Basic types
type RPM = Int -- rounds per minute

-- # Weapons
data Weapon = MkWeapon Projectile RPM | NoWeapon 

class Shootable a where
    shoot :: a -> Maybe Projectile

instance Shootable Weapon where
    shoot :: Weapon -> Maybe Projectile
    shoot NoWeapon = Nothing
    shoot (MkWeapon projectile rpm) = Just projectile -- #TODO MUST fire rate handling                                                                       andling                                                                       