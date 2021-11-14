{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
module Kinematics where

import Plane
import Data.Angle 
import Graphics.Gloss

type Acceleration = Vector
-- note that Target isn't explicitly used here, but it allows for pattern match against linear or homing movement 
type Time = Float
data Target x = MkTarget x | NoTarget
type Velocity = Vector



-- ### Motion equations
uniformLinearMotion :: Point -> Velocity -> Point
-- uniform linear motion x(t) = x_0 + v*t
uniformLinearMotion (x, y) (vx, vy) = let x' = x + vx
                                          y' = y + vy
                                          in (x', y')


-- uniformlyAcceleratedMotion :: Point -> Velocity -> Acceleration -> Time -> Point
-- -- uniformly accelerated motion x(t) = x_0 + v*t + 1/2 * at^2
-- uniformlyAcceleratedMotion (x, y) (vx, vy) (ax, ay) (Time { time }) = let x' = x + (vx * time) + (1/2) * (ax * (time ** 2.0))
--                                                                           y' = y + (vy * time) + (1/2) * (ay * (time ** 2.0))
--                                                                           in (x', y')position


homingMotion :: Point -> Velocity -> Degrees Float-> Point -> Velocity
-- homing motion, given starting location, current velocity vector, maximum turning angle and target return new velocity vector 
homingMotion position@(x,y) currVelocity (Degrees maxAngle) target@(x',y') = let shift = (x' - x, y' - y) -- shift from current position to target's position
                                                                       in case vecAngle currVelocity shift of -- angle between current velocity vector and shift vector
                                                                           Degrees angle | angle == 0 -> currVelocity
                                                                                         | angle > maxAngle -> polarToVec (polarVecAddAngle (vecToPolar currVelocity) (Degrees maxAngle))
                                                                                         | angle < (-1 * maxAngle) -> polarToVec (polarVecAddAngle (vecToPolar currVelocity) (Degrees ((-1) * maxAngle)))
                                                                           degrees -> polarToVec (polarVecAddAngle (vecToPolar position) degrees)

