module Player where

import Helper
import World


import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game

initPlayer :: Point -> Player
initPlayer (x, y) = MkPlayer (x, y)
                             3
                             False

instance Positioned Player where
    getPosition p = player_position p
    setPosition p pt = p {player_position = pt}

instance HitBox Player where
    getWidth  p = 1
    getHeight p = 1

instance RenderableObject Player where
    getColor p = red
    getPicture p = let w = getWidth p
                       h = getHeight p
                    in Line [(0, 0), (w, 0), (w, h), (0, h), (0, 0)] 

instance HandleInput Player where 
    handleInput player event = case event of
        EventKey (Char c) Down _ _      -> let (px, py) = getPosition player in
                                            case c of
                                                'c' -> setPosition player (px,py+1)
                                                'v' -> setPosition player (px,py-1)
                                                _   -> player
        event'                          -> player
