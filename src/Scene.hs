module Scene where

import TileMap
import GameObject

data Scene = Scene {
    hud      :: [GameObject],
    entities :: [GameObject],
    tileMap  :: TileMap
}