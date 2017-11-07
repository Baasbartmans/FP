-- | This module defines how the state changes
--   in response to time and user input
module Controller where
  
  import Model
  import Base
  import GameObject
  import Physics
  import Graphics.Gloss
  import Graphics.Gloss.Interface.IO.Game
  import System.Random
  import Graphics.Gloss.Data.Point
  import Scene
  
  -- | Handle one iteration of the game
  step :: Float -> GameState -> IO GameState
  step secs gstate
    | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
    = update secs gstate-- We show a new random number
      {-do randomNumber <- randomIO
         let newNumber = abs randomNumber `mod` 10
         return $ GameState (ShowANumber newNumber) 0-}
    | otherwise
    = -- Just update the elapsed time
      return $ gstate { elapsedTime = elapsedTime gstate + secs }
  
  -- | Handle user input
  input :: Event -> GameState -> IO GameState
  input e gstate = return (inputKey e gstate)
  
  inputKey :: Event -> GameState -> GameState
  inputKey (EventKey (MouseButton LeftButton) Down _ position) gstate@GameState{gsType=MainMenu} = 
    -- if button levelSelect is pressed go to level selection gamestate, else stay in the MainMenu gameState
    let object   = find "levelSelectBtn" (hud(scene gstate))
    in if   clickedOn position object
       then head $ gameStates gstate
       else gstate

  {-inputKey (EventKey (Char c) _ _ _) gstate
    = undefined-} -- If the user presses a character key, show that one
  inputKey _ gstate = gstate -- Otherwise keep the same

  clickedOn :: Position -> GameObject -> Bool
  clickedOn pos obj = let box        = collisionBox $ rigidBody obj
                          dimensions = size box
                          p1@(x,y)   = position box
                          p2         = (x + fromIntegral (fst dimensions), y + fromIntegral (snd dimensions))
                      in  pointInBox p1 p2 pos

  update :: Float -> GameState -> IO GameState
  update elapsedTime gstate@GameState{gsType=MainMenu} 
    = do randomNumber <- randomIO :: IO Int
         let newState = gstate
         return gstate