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
loadGameObject :: FilePath -> CollisionBox -> Float -> IO GameObject
loadGameObject path box weight = do sprite  <- loadBMP path
                                    let body = RigidBody box (0,0) (0,0) weight
                                    return $ GameObject body sprite

basicTile :: Position -> IO GameObject
basicTile pos = do loadGameObject "Assets/spr_idle.bmp" (CollisionBox pos (32,32)) 0