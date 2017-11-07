module ObjectFactory where

import Base
import Physics
import GameObject
import TileMap
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss

-- LOAD BITMAPS --------------------------------------------------------------------
loadBitMap :: FilePath -> IO Picture
loadBitMap path = do bmp <- loadBMP path
                     return bmp

-- LOAD TILEMAPS -------------------------------------------------------------------
-- read text from a file and transform it into a TileMap
loadTileMap :: FilePath -> IO TileMap
loadTileMap path = do content <- readFile path
                      tilemap <- traverseLines (lines content) 0
                      return tilemap

-- traverse all lines of a file and turn all IO [GameObject] (lines) into one IO [[GameObject]]
traverseLines :: [String] -> Int -> IO TileMap
traverseLines (x:xs) i = do line      <- traverseLine x (i, 0)
                            nextlines <- traverseLines xs (i+1)
                            return (line : nextlines)

-- traverse a line of a file and turn all IO GameObjects into one IO [GameObject]
traverseLine :: String -> (Int, Int) -> IO [GameObject]
traverseLine (z:zs) (x,y) = do tile      <- getTile z (fromIntegral x, fromIntegral y)
                               nexttiles <- traverseLine zs (x+1,y)
                               return (tile : nexttiles)

getTile :: Char -> Position -> IO GameObject
getTile char pos = case char of
                        '*' -> basicTile pos

-- GameObjects ---------------------------------------------------------------------
loadGameObject :: FilePath -> String -> CollisionBox -> Float -> IO GameObject
loadGameObject path name box weight = do sprite  <- loadBMP path
                                         let body = RigidBody box (0,0) (0,0) weight
                                         return $ GameObject name body sprite

basicTile :: Position -> IO GameObject
basicTile pos = do loadGameObject "Assets/spr_idle.bmp" 
                                  "basicTile" 
                                  (CollisionBox pos (32,32)) 
                                  0

groundTile :: Position -> IO GameObject
groundTile pos = do loadGameObject "Assets/spr_wall.bmp" 
                                  "groundTile #" 
                                  (CollisionBox pos (32,32)) 
                                  0

iceTile :: Position -> IO GameObject
iceTile pos = do loadGameObject "Assets/spr_ice.bmp" 
                                  "iceTile @" 
                                  (CollisionBox pos (32,32)) 
                                  0

hotTile :: Position -> IO GameObject
hotTile pos = do loadGameObject "Assets/spr_hot.bmp" 
                                  "hotTile +" 
                                  (CollisionBox pos (32,32)) 
                                  0

startTile :: Position -> IO GameObject
startTile pos = do loadGameObject "Assets/spr_idle.bmp" --geen sprite?
                                  "startTile 1" 
                                  (CollisionBox pos (32,32)) 
                                  0     

endTile :: Position -> IO GameObject
endTile pos = do loadGameObject "Assets/spr_goal.bmp" 
                                  "endTile X" 
                                  (CollisionBox pos (32,32)) 
                                  0

flameEnemy :: Position -> IO GameObject
flameEnemy pos = do loadGameObject "Assets/spr_flame@9.bmp" 
                                  "flameEnemy F" 
                                  (CollisionBox pos (32,32)) 
                                  0 

turtleEnemy :: Position -> IO GameObject
turtleEnemy pos = do loadGameObject "Assets/spr_sneeze@9.bmp" 
                                  "turtleEnemy T" 
                                  (CollisionBox pos (32,32)) 
                                  0 

rocketEnemyL :: Position -> IO GameObject
rocketEnemyL pos = do loadGameObject "Assets/spr_rocket@3.bmp" 
                                  "rocketEnemy L" 
                                  (CollisionBox pos (32,32)) 
                                  0
                            
rocketEnemyR :: Position -> IO GameObject
rocketEnemyR pos = do loadGameObject "Assets/spr_rocket@3.bmp" 
                                  "rocketEnemy R" 
                                  (CollisionBox pos (32,32)) 
                                  0 

sparkyEnemy :: Position -> IO GameObject
sparkyEnemy pos = do loadGameObject "Assets/spr_electrocute@6x5.bmp" 
                                  "sparkEnemy S" 
                                  (CollisionBox pos (32,32)) 
                                  0

waterPickup :: Position -> IO GameObject
waterPickup pos = do loadGameObject "Assets/spr_water.bmp" 
                                  "waterPickup W" 
                                  (CollisionBox pos (32,32)) 
                                  0     


