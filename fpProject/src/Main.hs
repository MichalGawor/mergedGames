{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Controller
import Model
import View

import Data.Maybe
import Data.Angle
import Objects.Enemy
import Objects.Objects
import Objects.Ships
import Graphics.Gloss
import Kinematics
import Plane

import WorldScreen
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Counter" (600, 600) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialModel     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function



-- sship = initSuicideEnemy (realToFrac 25, realToFrac 25)  

-- getPositionFromMaybeShip :: Positioned a => Maybe a -> Point 
-- getPositionFromMaybeShip (Just ship) = getPosition ship
-- getPositionFromMaybeShip Nothing = (-1, -1)

-- movedPosition = getPositionFromMaybeShip $ move sship (MkTarget target)
-- movedPosition' = getPositionFromMaybeShip $ move' sship (MkTarget target)


-- target = (realToFrac 0,realToFrac 0)
-- position = (realToFrac 25, realToFrac 25)  
-- velocity = (-1,0)
-- newVelocity =  homingMotion Main.position Main.velocity (Degrees (realToFrac 5)) target
-- newPosition = uniformLinearMotion Main.position newVelocity

-- move' :: Enemy -> Target Point -> Maybe Enemy
-- move' (MkSuicideEnemy (MkSuicideShip ship maxAngularSpeed)) (MkTarget (x, y)) = let 
--                                                                                     homingTrajectory = homingMotion Main.position Main.velocity maxAngularSpeed (x, y)
--                                                                                     newVelocity = homingTrajectory
--                                                                                     newPosition = uniformLinearMotion Main.position homingTrajectory
--                                                                                     in Just (MkSuicideEnemy (MkSuicideShip ship{ Objects.Ships.position = newPosition, Objects.Ships.velocity = newVelocity } maxAngularSpeed))


-- main :: IO ()
-- main = putStrLn (show newVelocity ++ "\n" ++ show newPosition ++ "\n" ++ show movedPosition ++ "\n" ++ show movedPosition')

          