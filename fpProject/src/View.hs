-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Helper

view :: GameModel -> IO Picture
view model = return $ renderModel model

renderModel :: GameModel -> Picture
renderModel model = render model (elapsedTime model)  
