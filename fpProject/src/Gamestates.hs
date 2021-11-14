module Gamestates where

import World
import WorldScreen
import Helper

data GameStates = World WorldState

initGameStates :: GameStates
initGameStates = World initWorldstate

instance Renderable GameStates where
    render (World wstate) secs = render wstate secs 

instance Updatable GameStates where
    update secs (World wstate) = World (update secs wstate)

instance HandleInput GameStates where
    handleInput (World wstate) e = World (handleInput wstate e) 

instance DoIO GameStates where
    doIO (World wstate) = do
        wstate' <- doIO wstate
        return $ World wstate'
    doIO gstate = return gstate
