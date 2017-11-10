module ObjectFactory where

import Base
import Physics
import TileMap
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap

-- LOAD BITMAPS --------------------------------------------------------------------
loadBitMap :: FilePath -> IO Picture
loadBitMap path = loadBMP path

-- LOAD TILEMAPS -------------------------------------------------------------------
-- read text from a file and transform it into a TileMap
loadTileMap :: FilePath -> IO TileMap
loadTileMap path = readFile path >>= traverseLines 0 . lines

-- traverse all lines of a file and turn all IO [GameObject] (lines) into one IO [[GameObject]]
traverseLines :: Int -> [String] -> IO TileMap
traverseLines i (x:xs) = (:) <$> (traverseLine x (i, 0)) <*> (traverseLines (i+1) xs)

-- traverse a line of a file and turn all IO GameObjects into one IO [GameObject]
traverseLine :: String -> (Int, Int) -> IO [GameObject]
traverseLine (z:zs) (x,y) = (:) <$> getTile z (fromIntegral x, fromIntegral y) <*> traverseLine zs (x+1,y)

getTile :: Char -> Position -> IO GameObject
getTile char pos = case char of
                        '*' -> basicTile pos

-- GameObjects ---------------------------------------------------------------------
loadGameObject :: FilePath -> String -> CollisionBox -> Float -> IO GameObject
loadGameObject path name box weight = let body = RigidBody box (0,0) (0,0) weight
                                      in  (\x -> GameObject name body x) <$> loadBMP path

basicTile :: Position -> IO GameObject
basicTile pos = loadGameObject "Assets/spr_idle.bmp" 
                               "basicTile" 
                               (CollisionBox pos (32,32)) 
                               0