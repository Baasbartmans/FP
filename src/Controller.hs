-- | This module defines how the state changes
--   in response to time and user input
module Controller where
  
  import Model
  import Base
  import Physics
  import TileMap
  import Screen
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
    update secs s m = do updatedObjects <- updateObjectList secs (entities s) m     
                         let maybePlayer        = find (\x -> "Player" == name x) updatedObjects
                         let newEntities        = if isJust maybePlayer
                                                  then updatePickups updatedObjects (getCollisionBox (fromJust maybePlayer))
                                                  else updatedObjects                                                
                         let tilemapbodies      = [getRigidBody obj | obj <- (concat (tileMap s)), not (isEmptyBody (body obj))]                         
                         let updatedRigidBodies = updateRigidBodies [rigidBody $ body obj | obj <- newEntities] tilemapbodies secs      --update rigidBodies
                         let finalObjects       = map (\(o, rb) -> o{body=(body o){rigidBody=rb}}) (zip newEntities updatedRigidBodies) --produce final objects
                         return s{entities=finalObjects}                                                                                   --create scene with same properties, but with updated entities 
  
  updateObjectList :: Float -> [GameObject] -> GameStateManager -> IO [GameObject]
  updateObjectList _ [] _          = return []
  updateObjectList secs l@(x:xs) m = (:) <$> update secs x m <*> updateObjectList secs xs m

  instance Updatable GameObject where
    update _ obj _ = return obj

  updatePickups :: [GameObject] -> CollisionBox -> [GameObject]
  updatePickups objects playerBox = let pickups  = [obj | obj <- objects, name obj == "Pickup"]
                                        pickedUp = filter (\x -> collides playerBox (getCollisionBox x)) pickups
                                    in  if   length pickedUp > 0
                                        then deletePickup pickups (head pickedUp)
                                        else objects

  deletePickup :: [GameObject] -> GameObject -> [GameObject]
  deletePickup (x:xs) obj | getCollisionBox x == getCollisionBox obj = xs
                          | otherwise                                = deletePickup xs obj

  -- | Handle one iteration of the game
  step :: Float -> GameStateManager -> IO GameStateManager
  step secs manager
     | elapsedTime manager + secs > nO_SECS_BETWEEN_CYCLES
     = do updatedGameState <- if gsType (current manager) == Play
                              then if anyPickupsLeft $ entities $ currentScene $ current manager
                                   then update nO_SECS_BETWEEN_CYCLES (current manager) manager
                                   else return $ getGameState manager SelectLevel
                              else return $ current manager
          
          return manager{current=updatedGameState, elapsedTime=0}
     | otherwise
     = return manager{elapsedTime = elapsedTime manager + secs}

  anyPickupsLeft :: [GameObject] -> Bool
  anyPickupsLeft objects | length (filter (\x -> "Pickup" == name x) objects) > 0 = True
                         | otherwise                                              = False

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
        lvl2Btn = buttonEvent mousePos "lvl2Btn" hudlist m{current=playState{currentScene=((scenes playState) !! 1)}}        
    in  getPressedButton [backBtn, lvl1Btn] m
  levelSelectInput _ m = m

  playInput :: Event -> GameStateManager -> GameStateManager
  playInput (EventKey (Char c) _ _ _) m = case c of 
                                            'w'       -> movePlayer (0  ,-15) m
                                            'a'       -> movePlayer (-15,  0) m
                                            's'       -> movePlayer (0  , 15) m
                                            'd'       -> movePlayer (15 ,  0) m
                                            otherwise -> m
  playInput _ m                         = m

  movePlayer :: (Float, Float) -> GameStateManager -> GameStateManager
  movePlayer move m = let gamestate  = current m
                          scene      = currentScene gamestate
                          objects    = entities scene
                          player     = fromJust $ find (\x -> name x == "Player") objects
                          rest       = replaceEntity "Player" objects
                          playerbody = body player
                          playerrb   = rigidBody playerbody
                          newPlayer  = player{body=playerbody{rigidBody=playerrb{addedMovement=move}}}
                          newObjects = newPlayer : rest
                      in  m{current=gamestate{currentScene=scene{entities=newObjects}}}

  replaceEntity :: String -> [GameObject] -> [GameObject]
  replaceEntity _ []     = []
  replaceEntity n (x:xs) | n == (name x) = xs
                         | otherwise     = replaceEntity n xs

  --AI
  data Direction = None | L | R | U | D deriving (Eq)

  inLineOfSight :: GameObject -> GameObject -> TileMap -> Bool
  inLineOfSight obj1 obj2 tilemap = let xpair  = getTileCoordinate obj1
                                        ypair  = getTileCoordinate obj2
                                        (inSameRow, direction, b1, b2) = getDirectionBoundsPair xpair ypair
                                    in  if inSameRow 
                                        then let rowToCheck = getRowToCheck tilemap direction b1
                                                 ints       = if   b1 < b2
                                                              then [b1..b2]
                                                              else [b2..b1]    
                                                 narrowDown = [(!!) rowToCheck i | i <- ints]
                                             in  checkDirection narrowDown                                                 
                                        else False

  getTileCoordinate :: GameObject -> (Int, Int)
  getTileCoordinate obj = let (x,y) = getPosition obj
                          in  (floor ( x / (fromIntegral tileWidth)), floor (y / (fromIntegral tileHeight)))
  
  getDirectionBoundsPair :: (Int, Int) -> (Int, Int) -> (Bool, Direction, Int, Int)
  getDirectionBoundsPair (x1, x2) (y1, y2) | x1 == x2  = if   x1 < x2
                                                         then (True, L, x1, x2)
                                                         else (True, R, x1, x2)
                                           | y1 == y2  = if   y1 < y2
                                                         then (True, U, y1, y2)
                                                         else (True, D, y1, y2)
                                           | otherwise = (False, None, 0, 0)

  getRowToCheck :: TileMap -> Direction -> Int -> [GameObject]
  getRowToCheck tilemap dir i | dir == U   || dir == D  = [(!!) row i| row <- tilemap]
                              | dir == L   || dir == R  = (!!) [[]] i --REPLACE WITH TILEMAP
                              | otherwise               = []

  checkDirection :: [GameObject] -> Bool
  checkDirection []     = True
  checkDirection (x:xs) | isEmptyBody $ body x = checkDirection xs
                        | otherwise            = False


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