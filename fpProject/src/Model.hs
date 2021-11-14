-- | This module contains the data types
--   which represent the state of the game
module Model where

import Gamestates
import Helper

import Objects.Ships
import Objects.Player
import Objects.Enemy
import Objects.Projectiles
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data Screen = MkScreen Float Float

data GameModel = MkGameModel { gameStates  :: GameStates,
                               elapsedTime :: Float,
                               screensize  :: Screen,
                               background  :: Picture}

instance Renderable GameModel where
    render model secs = Scale (width / 2) (height / 2) $ Pictures [background model, render (gameStates model) secs]  
                        where 
                          MkScreen width height = screensize model 

instance Updatable GameModel where
    update secs model = model { elapsedTime = elapsedTime model + secs, gameStates = update secs $ gameStates model } -- implement update like with renderable, so it calls update in gamestates -> gamestate calls update in current state -> current states updates everything


instance HandleInput GameModel where
    handleInput model event = case event of
        EventResize (x, y)  -> model {screensize = MkScreen (fromIntegral x) (fromIntegral y)}
        event               -> model {gameStates = handleInput (gameStates model) event}

instance DoIO GameModel where
    doIO model = do
        gstates <- doIO $ gameStates model
        return model {gameStates = gstates}

initialModel :: GameModel
initialModel = MkGameModel initGameStates 0 (MkScreen 600 600) Blank