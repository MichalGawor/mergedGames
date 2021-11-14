{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveAnyClass #-}
module Objects.Ships where 

import Data.Angle 
import Graphics.Gloss

import Kinematics
import Plane
import Objects.Objects
import Objects.Weapon

-- # Ships
--      Ship data record stores data fields shared by all the ships and implements its mortality
data Ship = Ship { 
    maxHp :: Hp,
    currHp :: Hp,
    weapon :: Weapon,
    position :: Point,
    velocity :: Velocity,
    collisionDamage :: Damage,
    getColor :: Color,
    getPicture :: Picture }

instance Killable Ship where
    takeDamage :: Ship -> Damage -> Maybe Ship
    takeDamage ship@(Ship { currHp, position, collisionDamage }) damage | currHp > damage = let newHp = currHp - damage 
                                                                                                in Just ship{ currHp = newHp, position = position, collisionDamage = collisionDamage }
                                                                        | otherwise = Nothing -- DEAL AREA DAMAGE HERE? EVENT HANDLING? 


instance Renderable Ship where
    render Ship { position, getColor, getPicture } = uncurry Graphics.Gloss.translate position (color getColor getPicture)

instance Positioned Ship where
    getPosition Ship {position} = position 
    setPosition ship@(Ship {position}) point = ship{position=point}

-- # Player's Ship
data PlayerShip = MkPlayerShip Ship

instance Moveable PlayerShip where
    move :: PlayerShip -> Target Point -> Maybe PlayerShip
    move pShip@(MkPlayerShip ship@(Ship { position })) (MkTarget (x, y)) | x == 1 = case uniformLinearMotion position (1, 0) of
                                                                                    newPosition | isInScreen newPosition -> Just (MkPlayerShip ship{position=newPosition})
                                                                                                | otherwise -> Just (MkPlayerShip ship)
                                                                         | x == -1 =case uniformLinearMotion position (-1, 0) of
                                                                                    newPosition | isInScreen newPosition -> Just (MkPlayerShip ship{position=newPosition})
                                                                                                | otherwise -> Just (MkPlayerShip ship)
    move pShip _ = Just pShip

instance Renderable PlayerShip where
    render (MkPlayerShip ship) = render ship  

instance Positioned PlayerShip where
    getPosition (MkPlayerShip ship) = getPosition ship 
    setPosition (MkPlayerShip ship) point = MkPlayerShip (setPosition ship point)                                                          

-- instance Moveable Enemy where
--     move (MkSuicideEnemy suicideShip) target = MkSuicideShip (move suicideShip target)
--     move (MkGunEnemy gunShip) target = MkGunEnemy (move gunShip target)
--     move (MkRocketEnemy rocketShip) target = MkRocketEnemy (move rocketShip target)
--     move (MkExplosion time) = undefined

-- ## Enemy ships
-- ## Gun ship
data GunShip = MkGunShip Ship -- deriving (Killable)

instance Moveable GunShip where
    move :: GunShip -> Target Point -> Maybe GunShip
    -- moves independent of the target, TODO keep distance on horizontal
    move (MkGunShip ship@( Ship { position, velocity })) _ = case uniformLinearMotion position velocity of 
                                                                 newPosition | isInScreen newPosition -> Just (MkGunShip ship{ position=newPosition, velocity=velocity })         
                                                                             | otherwise -> Nothing                                             

instance Renderable GunShip where
    render (MkGunShip ship) = render ship

instance Positioned GunShip where
    getPosition (MkGunShip ship) = getPosition ship
    setPosition (MkGunShip ship) point = MkGunShip $ setPosition ship point

-- ## Rocket ship
data RocketShip = MkRocketShip Ship -- deriving (Killable)

instance Moveable RocketShip where 
    move :: RocketShip -> Target Point -> Maybe RocketShip
    move (MkRocketShip ship@(Ship {position, velocity})) _ = case uniformLinearMotion position velocity of
                                                                 newPosition | isInScreen newPosition -> Just (MkRocketShip ship{ position=newPosition, velocity=velocity})
                                                                             | otherwise -> Nothing
                                                            
instance Renderable RocketShip where
    render (MkRocketShip ship) = render ship

instance Positioned RocketShip where
    getPosition (MkRocketShip ship) = getPosition ship
    setPosition (MkRocketShip ship) point = MkRocketShip $ setPosition ship point


-- ## Suicide ship
data SuicideShip = MkSuicideShip Ship (Degrees Float) -- deriving (Killable)

instance Moveable SuicideShip where
    -- moves straight if no target
    move :: SuicideShip -> Target Point -> Maybe SuicideShip
    move (MkSuicideShip ship@(Ship { position, velocity}) maxAngularSpeed) NoTarget = case uniformLinearMotion position velocity of
                                                                             newPosition | isInScreen newPosition -> Just (MkSuicideShip ship{ position = newPosition, velocity = velocity } maxAngularSpeed)
                                                                                         | otherwise -> Nothing
    -- track if target given
    move (MkSuicideShip ship@(Ship { position, velocity}) maxAngularSpeed) (MkTarget (x, y)) = let 
                                                                                                      homingTrajectory = homingMotion position velocity maxAngularSpeed (x, y)
                                                                                                      newVelocity = homingTrajectory
                                                                                                      newPosition = uniformLinearMotion position homingTrajectory 
                                                                                                      in case newPosition of
                                                                                                          newPosition | isInScreen newPosition -> Just (MkSuicideShip ship{ position = newPosition, velocity = newVelocity } maxAngularSpeed)
                                                                                                                      | otherwise -> Nothing

instance Renderable SuicideShip where
    render (MkSuicideShip ship _) = render ship

instance Positioned SuicideShip where
    getPosition (MkSuicideShip ship maxAngle) = getPosition ship
    setPosition (MkSuicideShip ship maxAngle) point = MkSuicideShip (setPosition ship point) maxAngle

-- TESTING
instance Show Ship where
    show :: Ship -> String
    show ship@( Ship{ position }) = show position

