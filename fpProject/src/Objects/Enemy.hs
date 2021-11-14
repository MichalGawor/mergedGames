module Objects.Enemy where

import Data.Angle

import Helper

import Objects.Weapon
import Objects.Ships
import Objects.Rocket
import Objects.Bullet
import Objects.Projectiles
import Objects.Objects

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game

instance HitBox Enemy where
    getWidth  _ = 1
    getHeight _ = 1

-- # Enemy datatype
data Enemy = MkSuicideEnemy SuicideShip | MkGunEnemy GunShip | MkRocketEnemy RocketShip

instance Renderable Enemy where
    render (MkSuicideEnemy suicideShip) = render suicideShip
    render (MkGunEnemy gunShip) = render gunShip
    render (MkRocketEnemy rocketShip) = render rocketShip

instance Positioned Enemy where
    getPosition (MkSuicideEnemy suicideShip) = getPosition suicideShip
    getPosition (MkRocketEnemy rocketShip) = getPosition rocketShip
    getPosition (MkGunEnemy gunShip) = getPosition gunShip
    setPosition (MkSuicideEnemy suicideShip) point = MkSuicideEnemy (setPosition suicideShip point)
    setPosition (MkRocketEnemy rocketShip) point = MkRocketEnemy $ setPosition rocketShip point
    setPosition (MkGunEnemy gunShip) point = MkGunEnemy $ setPosition gunShip point


initSuicideEnemy :: Point -> Enemy
initSuicideEnemy (x, y) = MkSuicideEnemy $ setPosition baseSuicideShip (x, y)

initRocketEnemy :: Point -> Enemy
initRocketEnemy (x, y) = MkRocketEnemy $ setPosition baseRocketSip (x, y)

initGunEnemy :: Point -> Enemy
initGunEnemy (x, y) = MkGunEnemy $ setPosition baseGunShip (x, y)

initProjectile :: Point -> Projectile
initProjectile (x, y) = undefined


baseShip :: Ship
baseShip = Ship {maxHp=100, currHp=100, weapon=NoWeapon, Objects.Ships.position=(50, 50), Objects.Ships.velocity=(-2.0, 0.0), collisionDamage=20, getColor=black, getPicture=(square 2.0) }

baseWeapon :: Weapon
baseWeapon = undefined

baseGunShip :: GunShip
baseGunShip = undefined

baseSuicideShip :: SuicideShip
baseSuicideShip = MkSuicideShip baseShip{ getColor=red, getPicture=circle 2.0} (Degrees 5)

baseRocketSip :: RocketShip
baseRocketSip = undefined

initPlayerShip :: PlayerShip
initPlayerShip = MkPlayerShip baseShip{ getColor=cyan, getPicture=rectangle 4.0 2.0, Objects.Ships.velocity=(0.0, 0.0)}




-- # SHAPES FOR SHIPS
square :: Float -> Picture
square side = rectangleSolid side side

rectangle :: Float -> Float -> Picture
rectangle sideA sideB = rectangleSolid sideA sideB