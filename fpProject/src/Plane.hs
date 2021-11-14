{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
module Plane where

import Graphics.Gloss
import Data.Angle
import Data.Fixed
-- ### Types
type PolarVector = (Float, (Degrees Float))
type ScreenWidth = Float -- Gloss uses Floats 
type ScreenHeight = Float -- Gloss uses Floats
type Resoulution = (ScreenWidth, ScreenHeight)

-- ### Vector operations
translate :: Vector -> Point -> Point
translate (x, y) (a, b) = (a + x, b + y)

-- ### Polar representation

vecAngle :: Vector -> Vector -> Degrees Float
-- get angle between two vectors in degrees
vecAngle (x, y) (x', y') = case shift (x, y) (x', y') of
                                    (sx, sy)   | sx>=0 && sy>=0 -> arccosine (cosA (x, y) (x', y')) -- I q
                                               | sx<0 && sy>=0 -> arccosine (cosA (x, y) (x', y')) -- IIq
                                               | sx<0 && sy<0 -> Degrees ( 360 - degreeToFloat (arccosine (cosA (x, y) (x', y')))) -- IIIq
                                               | sx>=0 && sy<0 -> arccosine (cosA (x, y) (x', y')) -- IVq

cosA (x, y) (x', y') = ((x * x') + (y * y')) / ((sqrt((x^2) + (y^2))) * (sqrt ((x' ^2) + (y' ^2))))


shift (x, y) (x', y') = (x' - x, y' - y)


degreeToFloat :: Degrees Float -> Float
degreeToFloat (Degrees x) = x

vecToPolar :: Vector -> PolarVector
-- represent vector in polar coordinater
vecToPolar (x, y) = let magnitude = (sqrt ((x^2) + (y^2)))
                          in (magnitude, (vecAngle (x, y) (1,0)))

polarToVec :: PolarVector -> Vector
-- represent polar coords as a vector
polarToVec (radius, angle) = (radius * (cosine angle), radius * (sine angle))

polarVecAddAngle :: PolarVector -> Degrees Float -> PolarVector
-- move polar by angle
polarVecAddAngle (radius, Degrees angle) (Degrees deltaAngle) = (radius, (Degrees (mod' (angle + deltaAngle) 360)))

-- ### Screen control
isInScreen :: Point -> Bool
isInScreen (x, y) | x > screenWidth || x > 0 || y < screenWidth || y > 0 = True
                  | otherwise = False

screenWidth :: ScreenWidth
screenWidth = 800

screenHeight :: ScreenHeight
screenHeight = 640
