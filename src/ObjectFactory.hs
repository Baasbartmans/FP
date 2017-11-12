module ObjectFactory where

import Base
import Physics
import TileMap
import Scene
import Screen
import Physics
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import Data.List
import Data.Maybe

-- LOAD BITMAPS --------------------------------------------------------------------
loadBitMap :: FilePath -> IO Picture
loadBitMap path = loadBMP path

-- LOAD LEVELS ---------------------------------------------------------------------
loadLevels :: [FilePath] -> IO[Scene]
loadLevels []     = return []
loadLevels (x:xs) = (:) <$> loadLevel x <*> loadLevels xs

loadLevel :: FilePath -> IO Scene
loadLevel path = do tilemap    <- loadTileMap path
                    let player  = head [obj | obj <- concat tilemap, name obj == "Player"]
                    let pickups = [obj | obj <- concat tilemap, name obj == "Pickup"]
                    let newMap1 = [[replaceWithEmpty "Player" obj | obj <- row] | row <- tilemap]
                    let newMap2 = [[replaceWithEmpty "Pickup" obj | obj <- row] | row <- newMap1]
                    return $ Scene [] (player : pickups) newMap2

replaceWithEmpty :: String -> GameObject -> GameObject
replaceWithEmpty n obj | n == (name obj) = GameObject "Empty" Empty
                       | otherwise       = obj

-- LOAD TILEMAPS -------------------------------------------------------------------
-- read text from a file and transform it into a TileMap
loadTileMap :: FilePath -> IO TileMap
loadTileMap path = readFile path >>= traverseLines 0 . lines

-- traverse all lines of a file and turn all IO [GameObject] (lines) into one IO [[GameObject]]
traverseLines :: Int -> [String] -> IO TileMap
traverseLines _ []     = return [[]]
traverseLines i (x:xs) = (:) <$> (traverseLine x (0, i)) <*> (traverseLines (i+1) xs)

-- traverse a line of a file and turn all IO GameObjects into one IO [GameObject]
traverseLine :: String -> (Int, Int) -> IO [GameObject]
traverseLine [] _         = return []
traverseLine (z:zs) (x,y) = (:) <$> getTile z (fromIntegral (x * tileWidth), fromIntegral (y * tileHeight)) <*> traverseLine zs (x+1,y)

getTile :: Char -> Position -> IO GameObject
getTile char pos = case char of
                        '.' -> basicTile
                        '#' -> groundTile pos
                        '1' -> startTile pos
                        'X' -> endTile pos
                        'F' -> flameEnemy pos
                        'W' -> waterPickup pos              

-- GameObjects ---------------------------------------------------------------------
loadGameObject :: FilePath -> String -> CollisionBox -> IO GameObject
loadGameObject path name box = let rigidbody = RigidBody box (0,0)
                                      in  (\x -> GameObject name (Body rigidbody x)) <$> loadBMP path

basicTile :: IO GameObject
basicTile = return $ GameObject "empty" Empty

groundTile :: Position -> IO GameObject
groundTile pos = do loadGameObject "Assets/spr_wall.bmp" 
                                   "groundTile #" 
<<<<<<< HEAD
                                   (CollisionBox True pos (72,55)) 
=======
                                   (CollisionBox (fst pos, snd pos) (64,64)) 
>>>>>>> 63fc6520ebb74e5b62e9a5ae4790821a9d9ea864

<<<<<<< HEAD
=======
iceTile :: Position -> IO GameObject
iceTile pos = do loadGameObject "Assets/spr_ice.bmp" 
                                  "iceTile @" 
                                  (CollisionBox True pos (32,32)) 

hotTile :: Position -> IO GameObject
hotTile pos = do loadGameObject "Assets/spr_hot.bmp" 
                                  "hotTile +" 
                                  (CollisionBox True pos (32,32)) 

>>>>>>> 94758a5ea23801aa870965d5e61ccbd0af02fc44
startTile :: Position -> IO GameObject
startTile pos = do loadGameObject "Assets/spr_idle.bmp" --geen sprite?
                                  "Player" 
<<<<<<< HEAD
                                  (CollisionBox True pos (112,94)) 
=======
                                  (CollisionBox (fst pos - 6 , snd pos - 6) (12,12)) 
>>>>>>> 63fc6520ebb74e5b62e9a5ae4790821a9d9ea864

endTile :: Position -> IO GameObject
endTile pos = do loadGameObject "Assets/spr_goal.bmp" 
                                  "endTile X" 
                                  (CollisionBox True pos (32,32)) 

flameEnemy :: Position -> IO GameObject
flameEnemy pos = do loadGameObject "Assets/spr_flame@9.bmp" 
                                  "flameEnemy F" 
                                  (CollisionBox True pos (32,32)) 

<<<<<<< HEAD
=======
turtleEnemy :: Position -> IO GameObject
turtleEnemy pos = do loadGameObject "Assets/spr_sneeze@9.bmp" 
                                  "turtleEnemy T" 
                                  (CollisionBox True pos (32,32)) 

rocketEnemyL :: Position -> IO GameObject
rocketEnemyL pos = do loadGameObject "Assets/spr_rocket@3.bmp" 
                                  "rocketEnemy L" 
                                  (CollisionBox True pos (32,32)) 
                            
rocketEnemyR :: Position -> IO GameObject
rocketEnemyR pos = do loadGameObject "Assets/spr_rocket@3.bmp" 
                                  "rocketEnemy R" 
                                  (CollisionBox True pos (32,32)) 

sparkyEnemy :: Position -> IO GameObject
sparkyEnemy pos = do loadGameObject "Assets/spr_electrocute@6x5.bmp" 
                                  "sparkyEnemy S" 
                                  (CollisionBox True pos (32,32)) 

>>>>>>> 94758a5ea23801aa870965d5e61ccbd0af02fc44
waterPickup :: Position -> IO GameObject
waterPickup pos = do loadGameObject "Assets/spr_water.bmp" 
                                  "Pickup" 
                                  (CollisionBox False pos (32,32)) 