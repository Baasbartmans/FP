module ObjectFactory where

import Base
import Physics
import GameObject
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import TileMap

-- LOAD BITMAPS --------------------------------------------------------------------
loadBitMap :: FilePath -> IO Picture
loadBitMap path = do bmp <- loadBMP path
                     return bmp

-- LOAD TILEMAPS -------------------------------------------------------------------
loadLevels :: [FilePath] -> IO [TileMap]
loadLevels paths = do sequence $ map (\path -> createTileMap path) paths

-- read text from a file and transform it into a TileMap
createTileMap :: FilePath -> IO TileMap
createTileMap path = do content <- readFile path
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

-- TILES ---------------------------------------------------------------------------
basicTile :: Position -> IO GameObject
basicTile pos = do  sprite          <- loadBitMap "../sprite"
                    let collisionbox = CollisionBox pos (4, 4) -- get size of sprite.... This is temporarily hardcoded
                    let rigidbody    = RigidBody collisionbox (0, 0) (0, 0) 0
                    return (GameObject rigidbody sprite)