module Objects.Enemy where

import World
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
data Enemy = MkSuicideEnemy SuicideShip | MkGunEnemy GunShip | MkRocketEnemy RocketShip deriving (Moveable)-- deriving (Moveable, Renderable, Killable)

instance Renderable Enemy where
    render (MkSuicideEnemy suicideShip) = render suicideShip
    render (MkGunEnemy gunShip) = render gunShip
    render (MkRocketEnemy rocketShip) = render rocketShip

instance Positioned Enemy where
    getPosition (MkSuicideShip suicideShip) = getPosition suicideShip
    getPosition (MkRocketShip rocketShip) = getPosition rocketShip
    getPosition (MkGunShip gunShip) = getPosition gunShip
    setPosition (MkSuicideShip suicideShip) point = MkSuicideShip $ setPosition ship point
    setPosition (MkRocketShip ship) point = MkRocketShip $ setPosition ship point
    setPosition (MkGunShip ship) point = MkGunShip $ setPosition ship point


initSucideEnemy :: Point -> Enemy
initSuicideEnemy (x, y) = setPosition baseSuicideShip (x, y)

initRocketEnemy :: Point -> Enemy
initRocketEnemy (x, y) = setPosition baseRocketSip (x, y)

initGunEnemy :: Point -> Enemy
initGunEnemy (x, y) = setPosition baseGunShip (x, y)

initProjectile :: Point -> Projectile
initProjectile (x, y) = undefined


baseShip :: Ship
baseShip = Ship {maxHp=100, currHp=100, weapon=NoWeapon, position=(50, 50), velocity=(-2.0, 0.0), collisionDamage=20, getColor=black, getPicture=(square 2.0) }

baseWeapon :: Weapon
baseWeapon = undefined

baseGunShip :: GunShip
baseGun = undefined

baseSuicideShip :: SuicideShip
baseSuicideShip = MkSuicideShip baseShip{ getColor=red, getPicture=circle 2.0} (Degrees 5)

baseRocketSip :: RocketShip
baseRocketSip = undefined

playerShip :: PlayerShip
playerShip = MkPlayerShip baseShip{ getColor=cyan, getPicture=rectangle 4.0 2.0, velocity=(0.0, 0.0)}




-- # SHAPES FOR SHIPS
square :: Float -> Picture
square side = rectangleSolid side side

rectangle :: Float -> Float -> Picture
rectangle sideA sideB = rectangleSolid sideA sideB