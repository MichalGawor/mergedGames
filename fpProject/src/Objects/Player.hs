{-# XTypeSynonymInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
module Objects.Player where

import Helper
import World

import Objects.Ships
import Objects.Objects
import Objects.Enemy
import Objects.Weapon
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game


initPlayer :: Point -> PlayerShip
initPlayer position = initPlayerShip position

initPlayerShip :: Point -> PlayerShip
initPlayerShip position = MkPlayerShip baseShip{ Objects.Ships.getShipColor=cyan, Objects.Ships.getShipPicture=rectangle 4.0 2.0, Objects.Ships.velocity=(0.0, 0.0), weapon=NoWeapon}


instance HitBox PlayerShip where
    getWidth  p = 1
    getHeight p = 1

instance HandleInput PlayerShip where 
    handleInput player event = case event of
        EventKey (Char c) Down _ _      -> let (px, py) = getPosition player in
                                            case c of
                                                'c' -> setPosition player (px,py+1)
                                                'v' -> setPosition player (px,py-1)
                                                _   -> player
        event'                          -> player