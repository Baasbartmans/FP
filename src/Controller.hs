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
  import View
  
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

  movement :: Event -> GameObject -> GameObject
  movement (EventKey (Char 'a') _ _ _) g            = walk g 0   -- walk left
  movement (EventKey (Char 'd') _ _ _) g            = walk g 1   -- walk right
  movement (EventKey (SpecialKey KeySpace) _ _ _) g = jump g 1 -- jump
  movement _   g                                    = g        -- do nothing

  jump :: GameObject -> Int -> GameObject
  jump g@GameObject{rigidBody = r} i | i < 10 && i > 0 = jump g{rigidBody = rigid' r 2} (i + 1)
                                     | i > 10          = jump g{rigidBody = rigid' r 3} (i - 1)
                                     | otherwise       = g

  walk :: GameObject -> Int -> GameObject
  walk g@GameObject{rigidBody = r} 0 = g{rigidBody = rigid' r 0}
  walk g@GameObject{rigidBody = r} 1 = g{rigidBody = rigid' r 1}   

                                
  rigid' :: RigidBody -> Int -> RigidBody
  rigid' r@RigidBody{collisionBox = c} 0 = r{collisionBox = collisionHP c}
  rigid' r@RigidBody{collisionBox = c} 1 = r{collisionBox = collisionHM c}
  rigid' r@RigidBody{collisionBox = c} 2 = r{collisionBox = collisionJU c}
  rigid' r@RigidBody{collisionBox = c} _ = r{collisionBox = collisionJD c}


  --aanpastypes

  collisionHP :: CollisionBox -> CollisionBox --Horizontal Plus
  collisionHP c@CollisionBox{position = p} = c{position = (pos', snd p)}
                                      where pos' = 1 + (fst p)

  collisionHM :: CollisionBox -> CollisionBox --Horizontal Min
  collisionHM c@CollisionBox{position = p} = c{position = (pos', snd p)}
                                      where pos' = (-1) + (fst p)

  collisionJU :: CollisionBox -> CollisionBox --Jump Up
  collisionJU c@CollisionBox{position = p} = c{position = (fst p, pos')}
                                      where pos' = 1 + (snd p)

  collisionJD :: CollisionBox -> CollisionBox --Jump down
  collisionJD c@CollisionBox{position = p} = c{position = (fst p, pos')}
                                      where pos' = (-1) + (fst p)


                                        
