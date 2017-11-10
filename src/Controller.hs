-- | This module defines how the state changes
--   in response to time and user input
module Controller where
  
  import Model
  import Base
  import Physics
  import Graphics.Gloss
  import Graphics.Gloss.Interface.IO.Game
  import System.Random
  import Graphics.Gloss.Data.Point
  import Scene
  
  -- | Handle one iteration of the game
  step :: Float -> GameStateManager -> IO GameStateManager
  step secs manager
     | elapsedTime manager + secs > nO_SECS_BETWEEN_CYCLES
     = do updatedGameState <- update nO_SECS_BETWEEN_CYCLES (current manager)
          return manager{current=updatedGameState, elapsedTime=0}
     | otherwise
     = return manager{elapsedTime = elapsedTime manager + secs}
  
  update :: Float -> GameState -> IO GameState
  update elapsedTime gstate = return gstate

  -- | Handle user input
  input :: Event -> GameStateManager -> IO GameStateManager
  input e manager = return $ inputKey e manager
  
  inputKey :: Event -> GameStateManager -> GameStateManager
    --MAIN MENU
  inputKey (EventKey (MouseButton LeftButton) Down _ mousePos) manager@GameStateManager{current=gstate@GameState{gsType=MainMenu}} = 
    let f1 = manager{current=getGameState manager SelectLevel}
    in  buttonEvent mousePos "levelSelectBtn" (hud(scene gstate)) f1 manager
  --LEVELSELECT MENU
  inputKey (EventKey (MouseButton LeftButton) Down _ mousePos) manager@GameStateManager{current=gstate@GameState{gsType=SelectLevel}} = 
    let f1 = manager{current=getGameState manager MainMenu}
    in  buttonEvent mousePos "backBtn" (hud(scene gstate)) f1 manager
  --REST
  inputKey _ manager = manager

  {-inputKey (EventKey (Char c) _ _ _) gstate
    = undefined-} -- If the user presses a character key, show that one
  --inputKey _ gstate = gstate -- Otherwise keep the same


  --BUTTONEVENTS
  buttonEvent :: Position -> String -> [GameObject] -> a -> a -> a
  buttonEvent pos name objects f1 f2 
    | clickedOn pos $ getCollisionBox $ find name objects = f1
    | otherwise                                           = f2

  clickedOn :: Position -> CollisionBox -> Bool
  clickedOn pos box = let dimensions = size box
                          (x,y)      = position box
                          p1@(p,q)   = (x - fromIntegral (fst dimensions) * 0.5, y - fromIntegral (snd dimensions) * 0.5)
                          p2         = (p + fromIntegral (fst dimensions), q + fromIntegral (snd dimensions))
                      in  pointInBox pos p1 p2