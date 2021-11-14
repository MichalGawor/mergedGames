
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Helper

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- | Handle one iteration of the game
step :: Float -> GameModel -> IO GameModel
step secs gstate = do
      gstate' <- doIO gstate
      return $ update secs gstate'

-- | Handle user input
input :: Event -> GameModel -> IO GameModel
input event gstate = return $ handleInput gstate event 