module Helper where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Picture

type Velocity = (Float, Float) 




class HandleInput a where
    handleInput :: a -> Event -> a                        

class Updatable a where
    update :: Float -> a -> a 

class DoIO a where
    doIO :: a -> IO a


