{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
module Kinematics where

import Plane
import Data.Angle (Angle, Degrees)
import Graphics.Gloss

type Acceleration = Vector
type AngularSpeed = Degrees Float
-- note that Target isn't explicitly used here, but it allows for pattern match against linear or homing movement 
type Time = Float
data Target x = MkTarget x | NoTarget
type Velocity = Vector



-- ### Motion equations
uniformLinearMotion :: Point -> Velocity -> Point
-- uniform linear motion x(t) = x_0 + v*t
uniformLinearMotion (x, y) (vx, vy) = let x' = x + (vx * 1)
                                          y' = y + (vy * 1)
                                          in (x', y')


-- uniformlyAcceleratedMotion :: Point -> Velocity -> Acceleration -> Time -> Point
-- -- uniformly accelerated motion x(t) = x_0 + v*t + 1/2 * at^2
-- uniformlyAcceleratedMotion (x, y) (vx, vy) (ax, ay) (Time { time }) = let x' = x + (vx * time) + (1/2) * (ax * (time ** 2.0))
--                                                                           y' = y + (vy * time) + (1/2) * (ay * (time ** 2.0))
--                                                                           in (x', y')


homingMotion :: Point -> Velocity -> AngularSpeed -> Point -> Velocity
-- homing motion, given starting location, current velocity vector, maximum turning angle and target return new velocity vector 
homingMotion position@(x,y) currVelocity maxAngle target@(x',y') = let shift = (x' - x, y' - y) -- shift from current position to target's position
                                                                       in case vecAngle currVelocity shift of -- angle between current velocity vector and shift vector
                                                                           angle | angle > maxAngle -> polarToVec (polarVecAddAngle (vecToPolar position) maxAngle)
                                                                           angle | angle < - maxAngle -> polarToVec (polarVecAddAngle (vecToPolar position) (-1 * maxAngle))
                                                                           angle -> polarToVec (polarVecAddAngle (vecToPolar position) angle)


