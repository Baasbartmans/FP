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
  import Data.Maybe
    
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
  inputKey e m | (gsType $ current m) == MainMenu    = mainMenuInput e m
               | (gsType $ current m) == SelectLevel = levelSelectInput e m
               | otherwise                           = m

  mainMenuInput :: Event -> GameStateManager -> GameStateManager
  mainMenuInput (EventKey (MouseButton LeftButton) Down _ mousePos) m =
    let f1             = m{current=getGameState m SelectLevel}
        levelSelectBtn = buttonEvent mousePos "levelSelectBtn" (hud (currentScene (current m))) f1
    in  getPressedButton [levelSelectBtn] m
  mainMenuInput _ m = m

  levelSelectInput :: Event -> GameStateManager -> GameStateManager
  levelSelectInput (EventKey (MouseButton LeftButton) Down _ mousePos) m =
    let hudlist = hud (currentScene(current m))
        backBtn = buttonEvent mousePos "backBtn" hudlist m{current=getGameState m MainMenu}
        playState = getGameState m Play
        lvl1Btn = buttonEvent mousePos "lvl1Btn" hudlist m{current=playState{currentScene=((scenes playState) !! 0)}}
        lvl2Btn = buttonEvent mousePos "lvl2Btn" hudlist m{current=playState{currentScene=((scenes playState) !! 1)}}        
    in  getPressedButton [lvl1Btn, backBtn] m
  levelSelectInput _ m = m

  {-inputKey (EventKey (Char c) _ _ _) gstate
    = undefined-} -- If the user presses a character key, show that one
  --inputKey _ gstate = gstate -- Otherwise keep the same

  --BUTTONEVENTS
  buttonEvent :: Position -> String -> [GameObject] -> a -> Maybe a
  buttonEvent pos name objects f1
    | clickedOn pos $ getCollisionBox $ find name objects = Just f1
    | otherwise                                           = Nothing

  clickedOn :: Position -> CollisionBox -> Bool
  clickedOn pos box = let dimensions = size box
                          (x,y)      = position box
                          p1@(p,q)   = (x - fromIntegral (fst dimensions) * 0.5, y - fromIntegral (snd dimensions) * 0.5)
                          p2         = (p + fromIntegral (fst dimensions), q + fromIntegral (snd dimensions))
                      in  pointInBox pos p1 p2

                      
  getPressedButton :: [Maybe GameStateManager] -> GameStateManager -> GameStateManager
  getPressedButton events m = let pressed = filter (\x -> isJust x) events
                              in  if   length pressed > 0
                                  then (fromJust . head) pressed
                                  else m