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
  import Data.List
    
  class Updatable a where
    update :: Float -> a -> GameStateManager -> IO a

  instance Updatable GameState where
    update secs s m = (\x -> s{currentScene = x}) <$> update secs (currentScene s) m
  
  instance Updatable Scene where
    update secs s m = do updatedObjects <- updateObjectList secs (entities s) m                --update objects
                         let updatedRigidBodies = updateRigidBodies [rigidBody $ body obj | obj <- updatedObjects] secs                  --update rigidBodies
                         let finalObjects       = map (\(o, rb) -> o{body=(body o){rigidBody=rb}}) (zip updatedObjects updatedRigidBodies) --produce final objects
                         return s{entities=finalObjects}                                                                                 --create scene with same properties, but with updated entities 
  
  updateObjectList :: Float -> [GameObject] -> GameStateManager -> IO [GameObject]
  updateObjectList _ [] _          = return []
  updateObjectList secs l@(x:xs) m = (:) <$> update secs x m <*> updateObjectList secs xs m

  instance Updatable GameObject where
    update secs obj m = return obj

  -- | Handle one iteration of the game
  step :: Float -> GameStateManager -> IO GameStateManager
  step secs manager
     | elapsedTime manager + secs > nO_SECS_BETWEEN_CYCLES
     = do updatedGameState <- update nO_SECS_BETWEEN_CYCLES (current manager) manager
          return manager{current=updatedGameState, elapsedTime=0}
     | otherwise
     = return manager{elapsedTime = elapsedTime manager + secs}

  -- | Handle user input
  input :: Event -> GameStateManager -> IO GameStateManager
  input e manager = return $ inputKey e manager
  
  inputKey :: Event -> GameStateManager -> GameStateManager
  inputKey e m | (gsType $ current m) == MainMenu    = mainMenuInput e m
               | (gsType $ current m) == SelectLevel = levelSelectInput e m
               | (gsType $ current m) == Play        = playInput e m
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
        --lvl2Btn = buttonEvent mousePos "lvl2Btn" hudlist m{current=playState{currentScene=((scenes playState) !! 1)}}        
    in  getPressedButton [backBtn, lvl1Btn] m
  levelSelectInput _ m = m

  playInput :: Event -> GameStateManager -> GameStateManager
  playInput (EventKey (Char c) _ _ _) m = case c of 
                                            'a'       -> movePlayer (-100,0) m
                                            'd'       -> movePlayer (100, 0) m
                                            otherwise -> m
  playInput _ m                         = m

  -- NOT WORKING
  movePlayer :: (Float, Float) -> GameStateManager -> GameStateManager
  movePlayer vel m = let gamestate  = current m
                         scene      = currentScene gamestate
                         objects    = entities scene
                         player     = fromJust $ find (\x -> name x == "Player") objects
                         rest       = delete player objects
                         playerbody = body player
                         playerrb   = rigidBody playerbody
                         newPlayer  = player{body=playerbody{rigidBody=playerrb{addedvelocity=vel}}}
                         newObjects = newPlayer : objects
                     in  m{current=gamestate{currentScene=scene{entities=newObjects}}}
  --BUTTONEVENTS
  buttonEvent :: Position -> String -> [GameObject] -> a -> Maybe a
  buttonEvent pos n objects f1
    | clickedOn pos $ getCollisionBox $ fromJust $ find (\x -> n == name x) objects = Just f1
    | otherwise                                                                     = Nothing

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