{-# XTypeSynonymInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

module Objects.Rocket where

import Graphics.Gloss
import Data.Angle
import Kinematics
import Plane
import Objects.Objects

data Rocket = MkRocket {
    position :: Point, 
    velocity :: Velocity, 
    damage :: Damage, 
    maxAngularSpeed :: Degrees Float, 
    existingTime :: Time
}

instance Moveable Rocket where
    move :: Rocket -> Target Point -> Maybe Rocket
    -- if no target move straight forward
    move rocket@(MkRocket { position, velocity }) NoTarget = let newPosition = uniformLinearMotion position velocity
                                                      in Just rocket{ position=newPosition, velocity=velocity }
    -- otherwise track the target
    move rocket@(MkRocket { position, velocity, maxAngularSpeed }) (MkTarget (x, y)) = let homingTrajectory = homingMotion position velocity maxAngularSpeed (x, y)
                                                                                           newVelocity = homingTrajectory
                                                                                           newPosition = uniformLinearMotion position homingTrajectory
                                                                                           in Just rocket{ position=newPosition, velocity=newVelocity, maxAngularSpeed=maxAngularSpeed }

instance RenderableM Rocket where
    renderM rocket = uncurry Graphics.Gloss.translate (position rocket) (color orange (Circle 0.3))

instance Positioned Rocket where
    getPosition rocket = position rocket
    setPosition rocket@( MkRocket {position}) newPosition = rocket{ position=newPosition }
