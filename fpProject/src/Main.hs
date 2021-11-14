module Main where

import Controller
import Model
import View

import Data.Maybe
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

-- position = getPosition sship

-- getPositionFromMaybeShip (Just ship) = getPosition ship
-- getPositionFromMaybeShip Nothing = (-1, -1)

-- movedShip = mapMaybe (flip Objects.Objects.move (MkTarget(2,2))) [sship]

-- newPosition = getPosition $ head movedShip

-- positions = show Main.position ++ "\n" ++ show newPosition
-- main :: IO ()
-- main = putStrLn positions

          