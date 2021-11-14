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



-- sship = initSuicideEnemy (25,25)


-- getPositionFromMaybeShip (Just ship) = getPosition ship
-- getPositionFromMaybeShip Nothing = (-1, -1)



-- target = (realToFrac 0,realToFrac 0)
-- position = (25,25)  
-- velocity = (-5,0)
-- newVelocity =  homingMotion Main.position Main.velocity (Degrees (realToFrac 5)) target

-- main :: IO ()
-- main = putStrLn (show newVelocity ++ "\n")

          